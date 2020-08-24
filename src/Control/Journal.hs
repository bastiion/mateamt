{-# LANGUAGE OverloadedStrings #-}
module Control.Journal where

import Servant

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Extra (anyM)

-- internal imports

import Types
import Model.Journal
import Control.Role (checkCapability)

journalShow
  :: Maybe (Int, AuthMethod)
  -> Maybe Int
  -> Maybe Int
  -> MateHandler [JournalEntry]
journalShow (Just (uid, method)) mlimit moffset = do
  maySeeJournal <- anyM
    (checkCapability uid)
    [roleCanViewJournal, roleCanManageJournal]
  if method `elem` [PrimaryPass, ChallengeResponse] && maySeeJournal
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
journalCheck (Just (uid, method)) check = do
  mayCheckJournal <- checkCapability uid roleCanManageJournal
  if method `elem` [PrimaryPass, ChallengeResponse] && mayCheckJournal
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
