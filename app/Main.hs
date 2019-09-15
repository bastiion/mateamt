{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.ByteString.Base16 (decode)

import Data.Set (empty)

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Control.Monad.Reader

import Control.Concurrent.STM.TVar

-- internal imports

import API
import Model as M

import Types
import Control

main :: IO ()
main = do
  conn <- connectPostgreSQL
    "host='localhost' port=5432 dbname='mateamt' user='mateamt' password='mateamt'"
  store <- newTVarIO empty
  void $ execute_ conn initAvatar
  void $ execute_ conn initUser
  void $ execute_ conn initProduct
  void $ execute_ conn initToken
  void $ execute_ conn initAuthData
  void $ execute_ conn initAmount
  void $ execute_ conn initJournal
  withStdoutLogger $ \ilog -> do
    let settings = setPort 8000 $ setLogger ilog defaultSettings
        initState = ReadState
          { rsConnection  = conn
          , rsTicketStore = store
          }
    runSettings settings (app initState)

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
      res <- case lookup "Authentication" headers of
        Just hh ->
          validateToken (fst $ decode hh) conn
        _       ->
          return Nothing
      return res
