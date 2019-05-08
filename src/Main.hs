{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Servant
import Servant.Server.Experimental.Auth

import Data.Time.Clock

import Data.ByteString.Random

import Data.Set (empty)

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
app initState = serveWithContext userApi genAuthServerContext $
  hoistServer userApi (`runReaderT` initState) users

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

users :: ServerT UserAPI (ReaderT ReadState Handler)
users =
  ( userList :<|>
    userNew :<|>
    userUpdate
  ) :<|>
  ( authGet :<|>
    authSend
  )
  where
    userList :: Maybe Refine -> Bool -> MateHandler [User]
    userList ref sw = liftIO $ do
      conn <- rsConnection <$> ask
      userSelect conn ref sw

    userNew :: UserSubmit -> MateHandler Int
    userNew  us = liftIO $ do
      now <- getCurrentTime
      randSalt <- random 8
      conn <- rsConnection <$> ask
      head <$> runInsert_ conn (insertUser us (utctDay now) randSalt)

    userUpdate :: (Int, UserSubmit) -> MateHandler ()
    userUpdate (id, us) = liftIO $ do
      now <- getCurrentTime
      conn <- rsConnection <$> ask
      void $ runUpdate_ conn (updateUser id us (utctDay now))

    authGet :: Int -> MateHandler AuthInfo
    authGet id =
      getUserAuthInfo id

    authSend :: AuthRequest -> MateHandler AuthResult
    authSend _ = liftIO $ Granted <$> AuthToken <$> random 8
