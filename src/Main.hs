{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant

import Data.Time.Clock

import Database.PostgreSQL.Simple

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
  run 3000 (app conn)

app :: Connection -> Application
app conn = serve userApi (users conn)

userApi :: Proxy UserAPI
userApi = Proxy

users :: Connection -> Server UserAPI
users conn =
  userList :<|>
  userNew :<|>
  userUpdate
  where
    userList :: Maybe Refine -> Handler [User]
    userList ref = liftIO $ userSelect conn ref
    userNew :: UserSubmit -> Handler Int
    userNew  us = liftIO $ do
      now <- getCurrentTime
      head <$> runInsert_ conn (insertUser us (utctDay now))
    userUpdate :: (Int, UserSubmit) -> Handler ()
    userUpdate (id, us) = liftIO $ do
      now <- getCurrentTime
      void $ runUpdate_ conn (updateUser id us (utctDay now))
