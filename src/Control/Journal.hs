{-# LANGUAGE OverloadedStrings #-}
module Control.Journal where

import Servant

import Control.Monad.Reader (ask)

-- internal imports

import Types
import Model.Journal

journalShow
  :: Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> MateHandler [JournalEntry]
journalShow (Just _) mlimit moffset = do
  conn <- rsConnection <$> ask
  selectJournalEntries mlimit moffset conn
journalShow Nothing _ _ =
  throwError $ err403
    { errBody = "No Authentication present"
    }
