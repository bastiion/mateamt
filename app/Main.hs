{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P

import Servant
import Servant.Server.Experimental.Auth

import Data.Set as S (empty)
import qualified Data.Map.Lazy as M
import Data.ByteString.Lazy as BL hiding (putStrLn)
import Data.ByteString.Char8 as B8 hiding (putStrLn)
import qualified Data.Text as T
import Data.String
import Data.YAML
import Data.Version (showVersion)
import Data.CaseInsensitive  (CI)
import qualified Data.CaseInsensitive as CI
import Data.IP

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Util

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Throttle
import Network.Socket (SockAddr(..), defaultPort)

import Control.Monad.Reader

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

import Options.Applicative

import System.Clock (TimeSpec(..))
import System.Exit

-- internal imports

import API
import Model as M

import AppTypes
import Types
import Control
import Janitor
-- import Middleware

import Paths_mateamt (version)

main :: IO ()
main = do
  (Options confLoc tMigLoc) <- execParser opts
  raw <- BL.readFile (T.unpack confLoc)
  case decode1 raw of
    Left (loc, msg) ->
      error (T.unpack $ confLoc <> ":" <>
        fromString (prettyPosWithSource loc raw " error") <>
        fromString msg
        )
    Right
      (ServerConfig
        db_host
        db_port
        db_name
        db_user
        db_passwd
        sym
        lport
        lhost
        -- max_conn_per_client
        block_registration
        ) -> do
          conn <- connectPostgreSQL (
            "host='" <> fromString (T.unpack db_host) <> "' " <>
            "port=" <> fromString (show db_port) <> " " <>
            "dbname='" <> fromString (T.unpack db_name) <> "' " <>
            "user='" <> fromString (T.unpack db_user) <> "' " <>
            "password='" <> fromString (T.unpack db_passwd) <> "'"
            )
          store <- newTVarIO S.empty
          -- tracker <- newTVarIO M.empty
          migrationsExist <- existsTable conn "schema_migrations"
          unless migrationsExist $ do
            withTransaction conn $
              void $ do
                runMigration $
                  MigrationContext MigrationInitialization True conn
                execute_ conn initAvatar
                execute_ conn initUser
                execute_ conn initProduct
                execute_ conn initToken
                execute_ conn initAuthData
                execute_ conn initAmount
                execute_ conn initJournal
                execute_ conn initRole
                execute_ conn initUserToRole
                void $ runInsertInitialRoles conn
          -- validate Migrations
          let migLoc = T.unpack tMigLoc
          ok <- withTransaction conn $ runMigration $ MigrationContext
            (MigrationValidation (MigrationDirectory migLoc)) True conn
          case ok of
            MigrationError err -> do
              putStrLn ("Migration validation error: " ++ err)
              putStrLn "Running Migrations!"
              void $ withTransaction conn $ runMigration $
                MigrationContext (MigrationDirectory migLoc) True conn
            MigrationSuccess -> return ()
          ok2 <- withTransaction conn $ runMigration $ MigrationContext
            (MigrationValidation (MigrationDirectory migLoc)) True conn
          case ok2 of
            MigrationError err -> do
              putStrLn ("Migration validation error: " ++ err)
              putStrLn "MIgration failure! exiting..."
              exitWith (ExitFailure 3)
            MigrationSuccess -> do
              putStrLn "Migration validation success!"
              putStrLn "starting up..."
          forkCleanProcess conn store
          withStdoutLogger $ \ilog -> do
            let settings = setPort (fromIntegral lport) $
                  setHost (fromString $ T.unpack lhost) $
                  -- setOnOpen (addToTracker tracker max_conn_per_client) $
                  -- setOnClose (removeFromTracker tracker) $
                  setLogger ilog defaultSettings
                initState = ReadState
                  { rsConnection  = conn
                  , rsTicketStore = store
                  , rsCurrencySymbol = sym
                  , rsSoftwareVersion = T.pack (showVersion version)
                  }
                expirationSpec = TimeSpec 5 0 -- five seconds
                throttleSettings = (defaultThrottleSettings expirationSpec)
                  { throttleSettingsRate   = 10
                  , throttleSettingsPeriod = 1000
                  }
            th <- initCustomThrottler throttleSettings
              (\req ->
                let headers = requestHeaders req
                in  case lookup "x-forwarded-for" headers of
                      Just addrs ->
                        let addr = fst (B8.break (== ',') addrs)
                        in  Right $ Address $ toSockAddr (read (B8.unpack addr), defaultPort)
                      Nothing -> Right $ Address $ remoteHost req
                )
            runSettings settings (throttle th (app block_registration initState))
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "Run the \"mateamt\" API-Server."
        <> header "mateamt - Your friendly mate distribution office"
        )

app :: Bool -> ReadState -> Application
-- app conn = serveWithContext userApi genAuthServerContext (users conn)
app block_registration initState =
  serveWithContext mateApi (genAuthServerContext (rsConnection initState)) $
    hoistServerWithContext
      mateApi
      authProxy
      (`runReaderT` initState)
      ( authGet :<|>
        authSend :<|>
        authLogout :<|>

        authManageList :<|>
        authManageNewAuth :<|>
        authManageDeleteAuth :<|>

        (
        if block_registration
        then
          const $ throwError $ err406
            { errBody = "User registration is not allowed."
            }
        else
          userNew
        ) :<|>
        userGet :<|>
        userUpdate :<|>
        userList :<|>
        userRecharge :<|>
        userTransfer :<|>

        productNew :<|>
        productOverview :<|>
        productStockRefill :<|>
        productStockUpdate :<|>
        productList :<|>
        productShortList :<|>

        buy :<|>

        journalShow :<|>
        journalCheck :<|>

        avatarGet :<|>
        avatarInsert :<|>
        avatarUpdate :<|>
        avatarList :<|>

        roleList :<|>
        roleNew :<|>
        roleUpdate :<|>
        roleDelete :<|>
        roleAssociationList :<|>
        roleAssociationSubmit :<|>
        roleAssociationDelete :<|>

        metaGet
      )

mateApi :: Proxy MateAPI
mateApi = Proxy

authProxy :: Proxy '[ AuthHandler Request (Maybe (Int, AuthMethod)) ]
authProxy = Proxy

genAuthServerContext
  :: Connection
  -> Context '[ AuthHandler Request (Maybe (Int, AuthMethod)) ]
genAuthServerContext conn = authHandler conn Servant.:. EmptyContext

type instance AuthServerData (AuthProtect "header-auth") = Maybe (Int, AuthMethod)

authHandler :: Connection -> AuthHandler Request (Maybe (Int, AuthMethod))
authHandler conn = mkAuthHandler handler
  where
    handler :: Request -> Handler (Maybe (Int, AuthMethod))
    handler req = do
      let headers = requestHeaders req
      case lookup "Authentication" headers of
        Just hh ->
          validateToken hh conn
        _       ->
          return Nothing
