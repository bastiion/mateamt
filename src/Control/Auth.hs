{-# LANGUAGE OverloadedStrings #-}
module Control.Auth where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (ask)

-- internal imports

import Types
import Model

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
