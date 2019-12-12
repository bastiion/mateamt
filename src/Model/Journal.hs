{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
module Model.Journal where

import Data.Maybe (isJust, fromJust)

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
  , "journal_id             SERIAL      NOT NULL,"
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
    , tableField "journal_total_amount"
    , tableField "journal_entry_is_check"
    )
  )


selectJournalEntries
  :: Maybe Int                    -- limit
  -> Maybe Int                    -- offset
  -> PGS.Connection
  -> MateHandler [JournalEntry]
selectJournalEntries mlimit moffset conn = liftIO $ do
  let lim = case mlimit of
        Just l  -> limit (l + 1)
        Nothing -> id
      off = case moffset of
        Just o  -> offset o
        Nothing -> id
  entries <- runSelect conn
    ( proc () -> do
      ret <- lim $ off $ orderBy (desc (\(id_, _, _, _, _) -> id_))
        (queryTable journalTable) -< ()
      returnA -< ret
      ) :: IO
        [ ( Int
          , UTCTime
          , T.Text
          , Int
          , Bool
          )
        ]
  return $ fst $ foldr
    (\(id_, ts, descr, tot, check) (fin, before)->
      (JournalEntry id_ descr ts (tot - before) tot check : fin, tot)
      )
    ( []
    , if isJust mlimit && length entries > fromJust mlimit
      then (\(_, _, _, x, _) -> x) (last entries)
      else 0
    )
    ( if isJust mlimit && length entries > fromJust mlimit
      then init entries
      else entries
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
        (jid, ts, descr, total, check) = head lastTwoEntries
    return $ Just $ JournalEntry jid descr ts diff total check
  else
    return Nothing


insertNewJournalEntry
  :: JournalSubmit
  -> PGS.Connection
  -> MateHandler Int
insertNewJournalEntry (JournalSubmit descr amount) conn = do
  lastTotal <- (\case
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
        , C.constant descr
        , C.constant (lastTotal + amount)
        , C.constant False
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }

insertNewCashCheck
  :: JournalCashCheck
  -> PGS.Connection
  -> MateHandler Int
insertNewCashCheck (JournalCashCheck amount) conn =
  -- lastTotal <- (\case
  --   Just j  -> journalEntryTotalAmount j
  --   Nothing -> 0
  --   ) <$> selectLatestJournalEntry conn
  liftIO $ do
    now <- getCurrentTime
    fmap head $ runInsert_ conn $ Insert
      { iTable     = journalTable
      , iRows      =
        [
        ( C.constant (Nothing :: Maybe Int)
        , C.constant now
        , C.constant ("Cash check" :: String)
        , C.constant amount
        , C.constant True
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }
