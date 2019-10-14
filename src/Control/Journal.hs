{-# LANGUAGE OverloadedStrings #-}
module Control.Journal where

import Servant

import Control.Monad.Reader (asks)

-- internal imports

import Types
import Model.Journal

journalShow
  :: Maybe (Int, AuthMethod)
  -> Maybe Int
  -> Maybe Int
  -> MateHandler [JournalEntry]
journalShow (Just _) mlimit moffset = do
  conn <- asks rsConnection
  selectJournalEntries mlimit moffset conn
journalShow Nothing _ _ =
  throwError $ err401
    { errBody = "No Authentication present"
    }
