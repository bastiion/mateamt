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
import Data.ByteString.Base16 (decode)

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
        beverages :<|>
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
          validateToken conn (fst $ decode hh)
        _       ->
          return Nothing
      return res

users =
  userList :<|>
  userNew :<|>
  userGetUpdate :<|>
  userPostUpdate
  where
    userList :: Maybe Int -> Maybe Refine -> MateHandler [User]
    userList muid ref = do
      conn <- rsConnection <$> ask
      userSelect conn ref

    userNew :: UserSubmit -> MateHandler Int
    userNew us = do
      now <- liftIO $ getCurrentTime
      randSalt <- liftIO $ random 8
      conn <- rsConnection <$> ask
      head <$> (liftIO $ runInsert_ conn (insertUser us (utctDay now) randSalt))

    userGetUpdate :: Maybe Int -> Int -> MateHandler UserDetails
    userGetUpdate Nothing _ =
      throwError $ err403
        { errBody = "No Authorization present"
        }
    userGetUpdate (Just aid) id =
      if aid == id
      then do
        now <- liftIO $ getCurrentTime
        conn <- rsConnection <$> ask
        -- void $ liftIO $ runUpdate_ conn (updateUser id us (utctDay now))
        userDetailsSelect conn id
      else
      throwError $ err403
        { errBody = "Wrong Authorization present"
        }

    userPostUpdate :: Maybe Int -> Int -> UserDetailsSubmit -> MateHandler ()
    userPostUpdate Nothing _ _ =
      throwError $ err403
        { errBody = "No Authorization present"
        }
    userPostUpdate (Just aid) id uds =
      if aid == id
      then do
        now <- liftIO $ getCurrentTime
        conn <- rsConnection <$> ask
        void $ liftIO $ runUpdate_ conn (updateUserDetails id uds (utctDay now))
      else
      throwError $ err403
        { errBody = "Wrong Authorization present"
        }

beverages =
  list :<|>
  new
  where
    list :: MateHandler [Beverage]
    list = do
      conn <- rsConnection <$> ask
      beverageSelect conn

    new :: Maybe Int -> BeverageSubmit -> MateHandler Int
    new _ bevsub = do
      conn <- rsConnection <$> ask
      head <$> (liftIO $ runInsert_ conn (insertBeverage bevsub))

auth =
  authGet :<|>
  authSend
  where
    authGet :: Int -> MateHandler AuthInfo
    authGet id =
      getUserAuthInfo id

    authSend :: AuthRequest -> MateHandler AuthResult
    authSend = processAuthRequest
