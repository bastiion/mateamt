{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.Time.Clock

import Data.ByteString.Random

import Data.Set (empty)

import Data.Maybe (isJust)

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Opaleye

import Control.Monad.IO.Class (liftIO)

import Control.Monad (void)

import Control.Monad.Reader

import Control.Concurrent.STM.TVar

-- internal imports

import API
import Model as M

import Types

main :: IO ()
main = do
  conn <- connectPostgreSQL
    "host='localhost' port=5432 dbname='mateamt' user='mateamt' password='mateamt'"
  store <- newTVarIO empty
  execute_ conn initUser
  execute_ conn initBeverage
  execute_ conn initToken
  withStdoutLogger $ \log -> do
    let settings = setPort 3000 $ setLogger log defaultSettings
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
      ( users :<|>
        auth
      )

userApi :: Proxy UserAPI
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
      res <- case lookup "Authorization" headers of
        Just hh -> do
          validateToken conn hh
        _       ->
          return Nothing
      return res

users =
  userList :<|>
  userNew :<|>
  userUpdate
  where
    userList :: Maybe Refine -> Maybe Int -> MateHandler [User]
    userList ref muid = do
      conn <- rsConnection <$> ask
      userSelect conn ref (isJust muid)

    userNew :: UserSubmit -> MateHandler Int
    userNew us = do
      now <- liftIO $ getCurrentTime
      randSalt <- liftIO $ random 8
      conn <- rsConnection <$> ask
      head <$> (liftIO $ runInsert_ conn (insertUser us (utctDay now) randSalt))

    userUpdate :: (Int, UserSubmit) -> MateHandler ()
    userUpdate (id, us) = do
      now <- liftIO $ getCurrentTime
      conn <- rsConnection <$> ask
      void $ liftIO $ runUpdate_ conn (updateUser id us (utctDay now))

auth =
  authGet :<|>
  authSend
  where
    authGet :: Int -> MateHandler AuthInfo
    authGet id =
      getUserAuthInfo id

    authSend :: AuthRequest -> MateHandler AuthResult
    authSend = processAuthRequest
