{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.Time.Clock

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Logger
import Network.Wai.Handler.Warp

import Opaleye

import Control.Monad.IO.Class (liftIO)

import Control.Monad (void)

-- internal imports

import API
import Model as M

import Types

main :: IO ()
main = do
  conn <- connectPostgreSQL
    "host='localhost' port=5432 dbname='mateamt' user='mateamt' password='mateamt'"
  execute_ conn initUser
  execute_ conn initBeverage
  withStdoutLogger $ \log -> do
    let settings = setPort 3000 $ setLogger log defaultSettings
    runSettings settings (app conn)

app :: Connection -> Application
app conn = serveWithContext userApi genAuthServerContext (users conn)

userApi :: Proxy UserAPI
userApi = Proxy

genAuthServerContext :: Context (AuthHandler Request Bool ': '[])
genAuthServerContext = authHandler Servant.:. EmptyContext

type instance AuthServerData (AuthProtect "header-auth") = Bool

authHandler :: AuthHandler Request Bool
authHandler = mkAuthHandler handler
  where
    handler req = do
      let headers = requestHeaders req
          res = case lookup "Authorization" headers of
            Just _ -> True
            _      -> False
      return res

users :: Connection -> Server UserAPI
users conn =
  ( userList :<|>
    userNew :<|>
    userUpdate
  ) :<|>
  ( authGet :<|>
    authSend
  )
  where
    userList :: Maybe Refine -> Bool -> Handler [User]
    userList ref sw = liftIO $ userSelect conn ref sw

    userNew :: UserSubmit -> Handler Int
    userNew  us = liftIO $ do
      now <- getCurrentTime
      head <$> runInsert_ conn (insertUser us (utctDay now))

    userUpdate :: (Int, UserSubmit) -> Handler ()
    userUpdate (id, us) = liftIO $ do
      now <- getCurrentTime
      void $ runUpdate_ conn (updateUser id us (utctDay now))

    authGet :: Int -> Handler AuthInfo
    authGet = liftIO . getUserAuthInfo conn

    authSend :: AuthRequest -> Handler AuthResult
    authSend _ = pure $ Granted $ AuthToken "mockgrant"
