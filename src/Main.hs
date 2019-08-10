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

import Opaleye hiding (max)

import Control.Monad.IO.Class (liftIO)

import Control.Monad (void)

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
  execute_ conn initUser
  execute_ conn initProduct
  execute_ conn initToken
  execute_ conn initAmount
  execute_ conn initJournal
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
      ( authGet :<|>
        authSend :<|>
        authLogout :<|>

        userNew :<|>
        userGet :<|>
        userUpdate :<|>
        userList :<|>

        productNew :<|>
        productOverview :<|>
        productStockRefill :<|>
        productStockUpdate :<|>
        productList :<|>

        buy :<|>

        journalShow
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
      res <- case lookup "Authentication" headers of
        Just hh ->
          validateToken conn (fst $ decode hh)
        _       ->
          return Nothing
      return res


authGet :: Int -> MateHandler AuthInfo
authGet id =
  getUserAuthInfo id

authSend :: AuthRequest -> MateHandler AuthResult
authSend = processAuthRequest

authLogout :: Maybe Int -> Int -> MateHandler ()
authLogout (Just muid) luid = do
  if muid == luid
  then
    processLogout luid
  else
    throwError $ err403
      { errBody = "Forbidden"
      }
authLogout Nothing _ = do
  throwError $ err403
    { errBody = "Forbidden"
    }


userNew :: UserSubmit -> MateHandler Int
userNew us = do
  now <- liftIO $ getCurrentTime
  randSalt <- liftIO $ random 8
  conn <- rsConnection <$> ask
  insertUser us (utctDay now) randSalt conn

userGet :: Maybe Int -> Int -> MateHandler UserDetails
userGet Nothing _ =
  throwError $ err403
    { errBody = "No Authentication present"
    }
userGet (Just aid) id =
  if aid == id
  then do
    now <- liftIO $ getCurrentTime
    conn <- rsConnection <$> ask
    -- void $ liftIO $ runUpdate_ conn (updateUser id us (utctDay now))
    userDetailsSelect conn id
  else
    throwError $ err403
      { errBody = "Wrong Authentication present"
      }

userUpdate :: Maybe Int -> Int -> UserDetailsSubmit -> MateHandler ()
userUpdate Nothing _ _ =
  throwError $ err403
    { errBody = "No Authentication present"
    }
userUpdate (Just aid) id uds =
  if aid == id
  then do
    now <- liftIO $ getCurrentTime
    conn <- rsConnection <$> ask
    void $ updateUserDetails id uds (utctDay now) conn
  else
    throwError $ err403
      { errBody = "Wrong Authentication present"
      }

userList :: Maybe Int -> Maybe Refine -> MateHandler [User]
userList muid ref = do
  conn <- rsConnection <$> ask
  userSelect conn ref


productNew :: Maybe Int -> ProductSubmit -> MateHandler Int
productNew (Just _) bevsub = do
  conn <- rsConnection <$> ask
  bevid <- insertProduct bevsub conn
  void $ insertNewEmptyAmount bevid bevsub conn
  return bevid
productNew Nothing _ =
  throwError $ err403

productOverview :: Int -> MateHandler ProductOverview
productOverview pid = do
  conn <- rsConnection <$> ask
  productOverviewSelectSingle pid conn

productStockRefill :: Maybe Int -> [AmountRefill] -> MateHandler ()
productStockRefill (Just _) amorefs = do
  if all ((>= 0) . amountRefillAmount) amorefs
  then do
    conn <- rsConnection <$> ask
    void $ manualProductAmountRefill amorefs conn
  else
    throwError $ err406
      { errBody = "Amounts less than 0 are not acceptable"
      }
productStockRefill Nothing _ =
  throwError $ err403
    { errBody = "No Authentication present"
    }

productStockUpdate :: Maybe Int -> [AmountUpdate] -> MateHandler ()
productStockUpdate (Just _) amoups = do
  if all ((>= 0) . amountUpdateRealAmount) amoups
  then do
    conn <- rsConnection <$> ask
    void $ manualProductAmountUpdate amoups conn
  else
    throwError $ err406
      { errBody = "Amounts less than 0 are not acceptable"
      }
productStockUpdate Nothing _ =
  throwError $ err403
    { errBody = "No Authentication present"
    }

productList :: MateHandler [ProductOverview]
productList = do
  conn <- rsConnection <$> ask
  productOverviewSelect conn
