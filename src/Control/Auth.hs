{-# LANGUAGE OverloadedStrings #-}
module Control.Auth where

import Servant

-- internal imports

import Types
import Model

authGet :: Int -> MateHandler AuthInfo
authGet uid =
  getUserAuthInfo uid

authSend :: AuthRequest -> MateHandler AuthResult
authSend = processAuthRequest

authLogout :: Maybe Int -> Int -> MateHandler ()
authLogout (Just muid) luid = do
  if muid == luid
  then
    processLogout luid
  else
    throwError $ err401
      { errBody = "Unauthorized access"
      }
authLogout Nothing _ = do
  throwError $ err401
    { errBody = "Unauthorized access"
    }
