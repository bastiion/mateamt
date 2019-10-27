{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.Set as S (empty)
import Data.ByteString.Lazy as BL hiding (putStrLn)
import qualified Data.Text as T
import Data.String
import Data.YAML

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Control.Monad.Reader

import Control.Concurrent.STM.TVar

import Options.Applicative

-- internal imports

import API
import Model as M

import AppTypes
import Types
import Control

main :: IO ()
main = do
  (Options confLoc) <- execParser opts
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
        ) -> do
          conn <- connectPostgreSQL (
            "host='" <> fromString (T.unpack db_host) <> "' " <>
            "port=" <> fromString (show db_port) <> " " <>
            "dbname='" <> fromString (T.unpack db_name) <> "' " <>
            "user='" <> fromString (T.unpack db_user) <> "' " <>
            "password='" <> fromString (T.unpack db_passwd) <> "'"
            )
          store <- newTVarIO S.empty
          void $ execute_ conn initAvatar
          void $ execute_ conn initUser
          void $ execute_ conn initProduct
          void $ execute_ conn initToken
          void $ execute_ conn initAuthData
          void $ execute_ conn initAmount
          void $ execute_ conn initJournal
          withStdoutLogger $ \ilog -> do
            let settings = setPort (fromIntegral lport) $
                  setHost (fromString $ T.unpack lhost) $
                  setLogger ilog defaultSettings
                initState = ReadState
                  { rsConnection  = conn
                  , rsTicketStore = store
                  }
            runSettings settings (app initState)
  where
    opts = info (options <**> helper)
      ( fullDesc
        <> progDesc "Run the \"mateamt\" API-Server."
        <> header "mateamt - Your friendly mate distribution office"
        )

app :: ReadState -> Application
-- app conn = serveWithContext userApi genAuthServerContext (users conn)
app initState =
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

        userNew :<|>
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

        avatarGet :<|>
        avatarInsert :<|>
        avatarUpdate :<|>
        avatarList
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
