{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Model.Journal where

import Data.Time (UTCTime)
import Data.Time.Clock

import qualified Data.Text as T

import Data.Profunctor.Product (p5)

import qualified Database.PostgreSQL.Simple as PGS

import Control.Arrow

import Opaleye as O hiding (null, not)
import Opaleye.Constant as C

import Control.Monad.IO.Class (liftIO)

-- internal imports

import Types

initJournal :: PGS.Query
initJournal = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"journal\" ("
  , "journal_id             SERIAL      NOT NULL ON DELETE CASCADE,"
  , "journal_timestamp      TIMESTAMPTZ NOT NULL,"
  , "journal_description    TEXT        NOT NULL,"
  , "journal_total_amount   INTEGER     NOT NULL,"
  , "journal_entry_is_check BOOLEAN     NOT NULL,"
  , "PRIMARY KEY (journal_id)"
  , ")"
  ]

journalTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlTimestamptz
  , Field SqlText
  , Field SqlInt4
  , Field SqlBool
  )
  ( Field SqlInt4
  , Field SqlTimestamptz
  , Field SqlText
  , Field SqlInt4
  , Field SqlBool
  )
journalTable = table "journal" (
  p5
    ( tableField "journal_id"
    , tableField "journal_timestamp"
    , tableField "journal_description"
    , tableField "journal_total_amout"
    , tableField "journal_entry_is_check"
    )
  )


selectLatestJournalEntry
  :: PGS.Connection
  -> MateHandler (Maybe JournalEntry)
selectLatestJournalEntry conn = liftIO $ do
  lastTwoEntries <- runSelect conn
    ( proc () -> do
      ret <- limit 2 (orderBy (desc (\(id_, _, _, _, _) -> id_))
        (queryTable journalTable)) -< ()
      returnA -< ret
      ) :: IO
        [ ( Int
          , UTCTime
          , T.Text
          , Int
          , Bool
          )
        ]
  if not (null lastTwoEntries)
  then do
    let diff = foldl (\acc (_, _, _, tot, _) -> tot - acc) 0 lastTwoEntries
        (id, ts, desc, total, check) = head lastTwoEntries
    return $ Just $ JournalEntry id desc ts diff total check
  else
    return Nothing


insertNewJournalEntry
  :: JournalSubmit
  -> PGS.Connection
  -> MateHandler Int
insertNewJournalEntry (JournalSubmit desc amount) conn = do
  lastTotal <- (\x -> case x of
    Just j  -> journalEntryTotalAmount j 
    Nothing -> 0
    ) <$> selectLatestJournalEntry conn
  liftIO $ do
    now <- getCurrentTime
    fmap head $ runInsert_ conn $ Insert
      { iTable     = journalTable
      , iRows      =
        [
        ( C.constant (Nothing :: Maybe Int)
        , C.constant now
        , C.constant desc
        , C.constant (lastTotal + amount)
        , C.constant False
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }
