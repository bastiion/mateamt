{-# LANGUAGE OverloadedStrings #-}
module Control.User where

import Servant

import Control.Monad (void, when)

import Control.Monad.Reader (asks)

import Control.Monad.IO.Class (liftIO)

import Data.Time (getCurrentTime, utctDay)

import Data.Maybe (fromMaybe)

import qualified Data.Text as T

-- internal imports

import Types
import Model
import Control.Role

userNew
  :: UserSubmit
  -> MateHandler Int
userNew (UserSubmit ident email passhash) = do
  now <- liftIO getCurrentTime
  conn <- asks rsConnection
  uid <- insertUser ident email (utctDay now) conn
  void $ putUserAuthInfo uid PrimaryPass "Initial password" passhash conn
  baseRoleId <- queryRoleIdByCapabilities
    (False, False, False, False, False, False, False, False, False, False)
    conn
  void $ associateUserToRole uid baseRoleId conn
  return uid

userGet
  :: Maybe (Int, AuthMethod)
  -> MateHandler UserDetails
userGet Nothing =
  throwError $ err401
    { errBody = "No Authentication present."
    }
userGet (Just (uid, _)) = do
  conn <- asks rsConnection
  userDetailsSelect uid conn

userUpdate
  :: Maybe (Int, AuthMethod)
  -> UserDetailsSubmit
  -> MateHandler ()
userUpdate Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }
userUpdate (Just (aid, method)) uds =
  if method `elem` [PrimaryPass, ChallengeResponse]
  then do
    now <- liftIO getCurrentTime
    conn <- asks rsConnection
    void $ updateUserDetails aid uds (utctDay now) conn
  else
    throwError $ err401
      { errBody = "Wrong Authentication present."
      }

userUpdateTimestamp
  :: Maybe (Int, AuthMethod)
  -> MateHandler ()
userUpdateTimestamp (Just (aid, _)) = do
  now <- liftIO getCurrentTime
  conn <- asks rsConnection
  void $ updateUserTimestamp aid (utctDay now) conn
userUpdateTimestamp Nothing =
  throwError $ err401
    { errBody = "No Authentication present."
    }

userList
  :: Maybe UserRefine
  -> MateHandler [UserSummary]
userList ref = do
  conn <- asks rsConnection
  userSelect (fromMaybe ActiveUsers ref) conn

userRecharge
  :: Maybe (Int, AuthMethod)
  -> UserRecharge
  -> MateHandler ()
userRecharge (Just (auid, _)) (UserRecharge amount) = do
  when (amount < 0) $
    throwError $ err400
      { errBody = "Amounts less or equal zero are not acceptable."
      }
  conn <- asks rsConnection
  ud <- userDetailsSelect auid conn
  void $ insertNewJournalEntry
    (JournalSubmit
      (Just $ userDetailsId ud)
      Recharge
      amount
      )
    conn
  void $ addToUserBalance auid amount conn
userRecharge Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

userTransfer
  :: Maybe (Int, AuthMethod)
  -> UserTransfer
  -> MateHandler ()
userTransfer (Just (auid, method)) (UserTransfer target amount) = do
  when (amount < 0) $
    throwError $ err400
      { errBody = "Amounts less or equal zero are not acceptable."
      }
  when (auid == target) $
    throwError $ err400
      { errBody = "You can not transfer yourself money."
      }
  when (method `notElem` [PrimaryPass, ChallengeResponse]) $
    throwError $ err401
      { errBody = "No Authentication present."
      }
  conn <- asks rsConnection
  user <- userDetailsSelect auid conn
  when (amount > userDetailsBalance user) $
    throwError $ err400
      { errBody = "Not enough credit balance."
      }
  mtarget <- filter (\u -> userSummaryId u == target) <$> userSelect AllUsers conn
  when (null mtarget) $
    throwError $ err400
      { errBody = "Target user not found."
      }
  void $ addToUserBalance auid (-amount) conn
  void $ addToUserBalance target amount conn
userTransfer Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }
