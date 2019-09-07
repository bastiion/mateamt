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
  void $ execute_ conn initAmount
  void $ execute_ conn initJournal
  withStdoutLogger $ \ilog -> do
    let settings = setPort 3000 $ setLogger ilog defaultSettings
        initState = ReadState
          { rsConnection  = conn
          , rsTicketStore = store
          }
    runSettings settings (app initState)

app :: ReadState -> Application
-- app conn = serveWithContext userApi genAuthServerContext (users conn)
app initState =
  serveWithContext userApi (genAuthServerContext (rsConnection initState)) $
    hoistServerWithContext
      userApi
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

userApi :: Proxy MateAPI
userApi = Proxy

authProxy :: Proxy '[ AuthHandler Request (Maybe Int) ]
authProxy = Proxy

genAuthServerContext
  :: Connection
  -> Context '[ AuthHandler Request (Maybe Int) ]
genAuthServerContext conn = authHandler conn Servant.:. EmptyContext

type instance AuthServerData (AuthProtect "header-auth") = Maybe Int

authHandler :: Connection -> AuthHandler Request (Maybe Int)
authHandler conn = mkAuthHandler handler
  where
    handler :: Request -> Handler (Maybe Int)
    handler req = do
      let headers = requestHeaders req
      res <- case lookup "Authentication" headers of
        Just hh ->
          validateToken conn (fst $ decode hh)
        _       ->
          return Nothing
      return res
