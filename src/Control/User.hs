{-# LANGUAGE OverloadedStrings #-}
module Control.User where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (ask)

import Control.Monad.IO.Class (liftIO)

import Data.Time (getCurrentTime, utctDay)

import Data.ByteString.Random (random)

import qualified Data.Text as T

-- internal imports

import Types
import Model

userNew :: UserSubmit -> MateHandler Int
userNew us = do
  now <- liftIO $ getCurrentTime
  randSalt <- liftIO $ random 8
  conn <- rsConnection <$> ask
  insertUser us (utctDay now) randSalt conn

userGet :: Maybe Int -> Int -> MateHandler UserDetails
userGet Nothing _ =
  throwError $ err403
    { errBody = "No Authentication present."
    }
userGet (Just aid) id =
  if aid == id
  then do
    now <- liftIO $ getCurrentTime
    conn <- rsConnection <$> ask
    -- void $ liftIO $ runUpdate_ conn (updateUser id us (utctDay now))
    userDetailsSelect id conn
  else
    throwError $ err403
      { errBody = "Wrong Authentication present."
      }

userUpdate :: Maybe Int -> Int -> UserDetailsSubmit -> MateHandler ()
userUpdate Nothing _ _ =
  throwError $ err403
    { errBody = "No Authentication present."
    }
userUpdate (Just aid) id uds =
  if aid == id
  then do
    now <- liftIO $ getCurrentTime
    conn <- rsConnection <$> ask
    void $ updateUserDetails id uds (utctDay now) conn
  else
    throwError $ err403
      { errBody = "Wrong Authentication present."
      }

userList :: Maybe Int -> Maybe Refine -> MateHandler [User]
userList muid ref = do
  conn <- rsConnection <$> ask
  userSelect ref conn

userRecharge :: Maybe Int -> UserRecharge -> MateHandler ()
userRecharge (Just auid) (UserRecharge amount) =
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
  throwError $ err403
    { errBody = "No Authentication present."
    }

userTransfer :: Maybe Int -> UserTransfer -> MateHandler ()
userTransfer (Just auid) (UserTransfer target amount) =
  if amount >= 0
  then
    if auid /= target
    then do
      conn <- rsConnection <$> ask
      user <- userDetailsSelect auid conn
      if amount < userDetailsBalance user
      then do
        mtarget <- filter (\u -> userId u == target) <$> userSelect (Just All) conn
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
         { errBody = "Not enough credit balance"
         }
    else
      throwError $ err400
        { errBody = "You can not transfer yourself money."
        }
  else
    throwError $ err400
      { errBody = "Amounts less or equal zero are not acceptable."
      }
userTransfer Nothing _ =
  throwError $ err403
    { errBody = "No Authentication present."
    }
