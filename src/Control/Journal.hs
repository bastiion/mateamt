{-# LANGUAGE OverloadedStrings #-}
module Control.Journal where

import Servant

import Control.Monad (void)
import Control.Monad.Reader (asks)

-- internal imports

import Types
import Model.Journal

journalShow
  :: Maybe (Int, AuthMethod)
  -> Maybe Int
  -> Maybe Int
  -> MateHandler [JournalEntry]
journalShow (Just (_, method)) mlimit moffset =
  if method `elem` [PrimaryPass, ChallengeResponse]
  then do
    conn <- asks rsConnection
    selectJournalEntries mlimit moffset conn
  else
    throwError $ err401
      { errBody = "Wrong Authentication present"
      }
journalShow Nothing _ _ =
  throwError $ err401
    { errBody = "No Authentication present"
    }

journalCheck
  :: Maybe (Int, AuthMethod)
  -> JournalCashCheck
  -> MateHandler ()
journalCheck (Just (_, method)) check =
  if method `elem` [PrimaryPass, ChallengeResponse]
  then do
    conn <- asks rsConnection
    void $ insertNewCashCheck check conn
  else
    throwError $ err401
      { errBody = "Wrong Authentication present"
      }
journalCheck Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present"
    }
