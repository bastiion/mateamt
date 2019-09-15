{-# LANGUAGE OverloadedStrings #-}
module Control.Auth where

import Servant

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM (readTVarIO)

-- internal imports

import Types
import Model

authGet
  :: TicketRequest
  -> MateHandler AuthInfo
authGet (TicketRequest uid method) = do
  getUserAuthInfo uid method =<< (rsConnection <$> ask)

authSend
  :: AuthRequest
  -> MateHandler AuthResult
authSend req = uncurry (processAuthRequest req) =<< ((,) <$>
  (liftIO . readTVarIO =<< rsTicketStore <$> ask) <*>
  (rsConnection <$> ask)
  )

authLogout
  :: Maybe (Int, AuthMethod)
  -> MateHandler ()
authLogout (Just (muid, method)) = do
  processLogout muid =<< (rsConnection <$> ask)
authLogout Nothing = do
  throwError $ err401
    { errBody = "Unauthorized access"
    }
