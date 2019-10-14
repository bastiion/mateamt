{-# LANGUAGE OverloadedStrings #-}
module Control.Auth where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (asks, ask)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM (readTVarIO)

-- internal imports

import Types
import Model

authGet
  :: TicketRequest
  -> MateHandler AuthInfo
authGet (TicketRequest uid method) =
  getUserAuthInfo uid method =<< (asks rsConnection)

authSend
  :: AuthRequest
  -> MateHandler AuthResult
authSend req = uncurry (processAuthRequest req) =<< ((,) <$>
  (liftIO . readTVarIO =<< rsTicketStore <$> ask) <*>
  (asks rsConnection)
  )

authLogout
  :: Maybe (Int, AuthMethod)
  -> MateHandler ()
authLogout (Just (muid, _)) =
  processLogout muid =<< (asks rsConnection)
authLogout Nothing =
  throwError $ err401
    { errBody = "Unauthorized access"
    }

authManageList
  :: Maybe (Int, AuthMethod)
  -> MateHandler [AuthOverview]
authManageList (Just (uid, method)) =
  if elem method [PrimaryPass, ChallengeResponse]
  then do
    conn <- asks rsConnection
    selectAuthOverviews uid conn
  else
    throwError $ err401
      { errBody = "Unauthorized access"
      }
authManageList Nothing =
  throwError $ err401
    { errBody = "Unauthorized access"
    }

authManageNewAuth
  :: Maybe (Int, AuthMethod)
  -> AuthSubmit
  -> MateHandler Int
authManageNewAuth (Just (uid, method)) (AuthSubmit asmethod ascomment aspayload) =
  if elem method [PrimaryPass, ChallengeResponse]
  then do
    conn <- asks rsConnection
    putUserAuthInfo uid asmethod ascomment aspayload conn
  else
    throwError $ err401
      { errBody = "Unauthorized access"
      }
authManageNewAuth Nothing _ =
  throwError $ err401
    { errBody = "Unauthorized access"
    }

authManageDeleteAuth
  :: Maybe (Int, AuthMethod)
  -> Int
  -> MateHandler ()
authManageDeleteAuth (Just (uid, method)) adid =
  if elem method [PrimaryPass, ChallengeResponse]
  then do
    conn <- asks rsConnection
    ads <- selectAuthOverviews uid conn
    let currentad = head (filter (\ad -> authOverviewId ad == adid) ads)
    case authOverviewMethod currentad of
      PrimaryPass -> if validateDeletion ads
        then void (deleteAuthDataById adid conn)
        else throwUnacceptableDeletionError
      ChallengeResponse -> if validateDeletion ads
        then void (deleteAuthDataById adid conn)
        else throwUnacceptableDeletionError
      _ -> void $ deleteAuthDataById adid conn
  else
    throwError $ err401
      { errBody = "Unauthorized access"
      }
  where
    validateDeletion ads =
      2 <= length (filter
        (\ad -> authOverviewMethod ad == PrimaryPass ||
          authOverviewMethod ad == ChallengeResponse)
        ads
        )
    throwUnacceptableDeletionError =
      throwError $ err406
        { errBody = "You need at least one primary password or challenge response authentication"
        }
authManageDeleteAuth Nothing _ =
  throwError $ err401
    { errBody = "Unauthorized access"
    }
