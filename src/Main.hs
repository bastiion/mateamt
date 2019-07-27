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

main :: IO ()
main = do
  conn <- connectPostgreSQL
    "host='localhost' port=5432 dbname='mateamt' user='mateamt' password='mateamt'"
  store <- newTVarIO empty
  execute_ conn initUser
  execute_ conn initProduct
  execute_ conn initToken
  execute_ conn initAmount
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
        products :<|>
        buy :<|>
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
      res <- case lookup "Authentication" headers of
        Just hh ->
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
        { errBody = "No Authentication present"
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
          { errBody = "Wrong Authentication present"
          }

    userPostUpdate :: Maybe Int -> Int -> UserDetailsSubmit -> MateHandler ()
    use359c65b0e68b6607a03d39f908ca26827ab97fb6e21096rPostUpdate Nothing _ _ =
      throwError $ err403
        { errBody = "No Authentication present"
        }
    userPostUpdate (Just aid) id uds =
      if aid == id
      then do
        now <- liftIO $ getCurrentTime
        conn <- rsConnection <$> ask
        void $ liftIO $ runUpdate_ conn (updateUserDetails id uds (utctDay now))
      else
        throwError $ err403
          { errBody = "Wrong Authentication present"
          }

products =
  list :<|>
  new :<|>
  update
  where
    list :: MateHandler [Product]
    list = do
      conn <- rsConnection <$> ask
      productSelect conn

    new :: Maybe Int -> ProductSubmit -> MateHandler Int
    new (Just _) bevsub = do
      conn <- rsConnection <$> ask
      now <- liftIO $ getCurrentTime
      bevid <- head <$> (liftIO $ runInsert_ conn (insertProduct bevsub))
      void $ liftIO $ runInsert_ conn (insertNewEmptyAmount bevid now bevsub)
      return bevid
    new Nothing _ =
      throwError $ err403

    update :: Maybe Int -> Int -> AmountUpdate -> MateHandler ()
    update (Just _) bid amosub = do
      conn <- rsConnection <$> ask
      liftIO $ do
        now <- getCurrentTime
        void $ runInsert_ conn (manualProductAmountUpdate amosub now bid)
    update Nothing _ _ =
      throwError $ err403

buy :: Maybe Int -> [PurchaseDetail] -> MateHandler PurchaseResult
buy (Just auid) pds = do
  conn <- rsConnection <$> ask
  (missing, real) <- foldM (\acc@(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {pdAmount = miss}):ms
        , (pd {pdAmount = max 0 (pdAmount pd - miss)}:rs)
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  void $ mapM_ (\pd -> postBuyProductAmountUpdate pd conn) real
  price <- foldM
    (\total pd ->
      fmap (+ total) (getLatestTotalPrice pd conn)
    )
    0
    real
  liftIO $ runUpdate_ conn (addToUserBalance auid (-price))
  newBalance <- userBalanceSelect conn auid
  return $ PurchaseResult
    ( if newBalance < 0
      then PurchaseDebtful
      else PurchaseOK
    )
    missing
buy Nothing pds = do
  conn <- rsConnection <$> ask
  (missing, real) <- foldM (\acc@(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {pdAmount = miss}):ms
        , (pd {pdAmount = max 0 (pdAmount pd - miss)}:rs)
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  void $ mapM_
    (\pd -> postBuyProductAmountUpdate pd conn)
    real
  price <- foldM
    (\total pd ->
      fmap (+ total) (getLatestTotalPrice pd conn)
    )
    0
    real
  return $ PurchaseResult
    (PayAmount price)
    missing

auth =
  authGet :<|>
  authSend :<|>
  authLogout
  where
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
