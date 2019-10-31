{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.Set as S (empty)
import qualified Data.Map.Lazy as M
import Data.ByteString.Lazy as BL hiding (putStrLn)
import qualified Data.Text as T
import Data.String
import Data.YAML

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp
import Network.Socket (SockAddr(..))

import Control.Monad.Reader

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

import Options.Applicative

-- internal imports

import API
import Model as M

import AppTypes
import Types
import Control
import Janitor

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
        max_conn_per_client
        ) -> do
          conn <- connectPostgreSQL (
            "host='" <> fromString (T.unpack db_host) <> "' " <>
            "port=" <> fromString (show db_port) <> " " <>
            "dbname='" <> fromString (T.unpack db_name) <> "' " <>
            "user='" <> fromString (T.unpack db_user) <> "' " <>
            "password='" <> fromString (T.unpack db_passwd) <> "'"
            )
          store <- newTVarIO S.empty
          tracker <- newTVarIO M.empty
          void $ execute_ conn initAvatar
          void $ execute_ conn initUser
          void $ execute_ conn initProduct
          void $ execute_ conn initToken
          void $ execute_ conn initAuthData
          void $ execute_ conn initAmount
          void $ execute_ conn initJournal
          forkCleanProcess conn store
          withStdoutLogger $ \ilog -> do
            let settings = setPort (fromIntegral lport) $
                  setHost (fromString $ T.unpack lhost) $
                  setOnOpen (addToTracker tracker max_conn_per_client) $
                  setOnClose (removeFromTracker tracker) $
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

addToTracker
  :: TVar (M.Map (Word, Word, Word, Word) Word)
  -> Word
  -> SockAddr
  -> IO Bool
addToTracker tracker maxconn saddr = do
  let laddr = translateAddr saddr
  (nmap, accept) <- atomically $ do
    tmap <- readTVar tracker
    let (nmap, accept) = case tmap M.!? laddr of
          Just val ->
            if val < maxconn
            then
              (M.adjust (const $ val + 1) laddr tmap, True)
            else
              (tmap, False)
          Nothing ->
            (M.insert laddr 1 tmap, True)
    writeTVar tracker nmap
    return (nmap, accept)
  print nmap
  return accept

translateAddr
  :: (Num a, Num b, Num c, Num d)
  => SockAddr
  -> (a, b, c, d)
translateAddr saddr =
  case saddr of
    SockAddrInet _ addr -> (0, 0, 0, fromIntegral addr)
    SockAddrInet6 _ _ (a1, a2, a3, a4) _ ->
      (fromIntegral a1, fromIntegral a2, fromIntegral a3, fromIntegral a4)
    _ -> error "Not made for sockets and the like"

removeFromTracker
  :: TVar (M.Map (Word, Word, Word, Word) Word)
  -> SockAddr
  -> IO ()
removeFromTracker tracker saddr = do
  let laddr = translateAddr saddr
  atomically $ do
    tmap <- readTVar tracker
    let num = tmap M.! laddr
        nmap = if num -1 > 0
          then
            M.adjust (const $ num - 1) laddr tmap
          else
            M.delete laddr tmap
    writeTVar tracker nmap

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
