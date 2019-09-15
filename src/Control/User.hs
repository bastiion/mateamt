{-# LANGUAGE OverloadedStrings #-}
module Control.User where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (ask)

import Control.Monad.IO.Class (liftIO)

import Data.Time (getCurrentTime, utctDay)

import Data.Maybe (fromMaybe)

import qualified Data.Text as T

-- internal imports

import Types
import Model

userNew
  :: UserSubmit
  -> MateHandler Int
userNew us = do
  now <- liftIO $ getCurrentTime
  conn <- rsConnection <$> ask
  insertUser us (utctDay now) conn

userGet
  :: Maybe (Int, AuthMethod)
  -> Int
  -> MateHandler UserDetails
userGet Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }
userGet (Just (aid, method)) uid =
  if aid == uid && any (== method) [PrimaryPass, ChallengeResponse]
  then do
    conn <- rsConnection <$> ask
    userDetailsSelect uid conn
  else
    throwError $ err401
      { errBody = "Wrong Authentication present."
      }

userUpdate
  :: Maybe (Int, AuthMethod)
  -> Int
  -> UserDetailsSubmit
  -> MateHandler ()
userUpdate Nothing _ _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }
userUpdate (Just (aid, method)) uid uds =
  if aid == uid && any (== method) [PrimaryPass, ChallengeResponse]
  then do
    now <- liftIO $ getCurrentTime
    conn <- rsConnection <$> ask
    void $ updateUserDetails uid uds (utctDay now) conn
  else
    throwError $ err401
      { errBody = "Wrong Authentication present."
      }

userList
  :: Maybe UserRefine
  -> MateHandler [UserSummary]
userList ref = do
  conn <- rsConnection <$> ask
  userSelect (fromMaybe ActiveUsers ref) conn

userRecharge
  :: Maybe (Int, AuthMethod)
  -> UserRecharge
  -> MateHandler ()
userRecharge (Just (auid, _)) (UserRecharge amount) =
  if amount >= 0
  then do
    conn <- rsConnection <$> ask
    ud <- userDetailsSelect auid conn
    void $ insertNewJournalEntry
      (JournalSubmit
        ("User \"" <> userDetailsIdent ud <> "\" recharged " <>
          T.pack (show (fromIntegral amount / 100 :: Double)))
        amount
        )
      conn
    void $ addToUserBalance auid amount conn
  else
    throwError $ err400
      { errBody = "Amounts less or equal zero are not acceptable."
      }
userRecharge Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

userTransfer
  :: Maybe (Int, AuthMethod)
  -> UserTransfer
  -> MateHandler ()
userTransfer (Just (auid, method)) (UserTransfer target amount) =
  if amount >= 0
  then
    if auid /= target
    then
      if any (== method) [PrimaryPass, ChallengeResponse]
      then do
        conn <- rsConnection <$> ask
        user <- userDetailsSelect auid conn
        if amount < userDetailsBalance user
        then do
          mtarget <- filter (\u -> userSummaryId u == target) <$> userSelect AllUsers conn
          if not (null mtarget)
          then do
            void $ addToUserBalance auid (-amount) conn
            void $ addToUserBalance target amount conn
          else
            throwError $ err400
              { errBody = "Target user not found."
              }
        else
         throwError $ err400
           { errBody = "Not enough credit balance."
           }
      else
        throwError $ err400
          { errBody = "You can not transfer yourself money."
          }
    else
      throwError $ err401
        { errBody = "No Authentication present."
        }
  else
    throwError $ err400
      { errBody = "Amounts less or equal zero are not acceptable."
      }
userTransfer Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }
