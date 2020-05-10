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
  , "journal_user           INTEGER,"
  , "journal_action         INTEGER     NOT NULL,"
  , "journal_total_amount   INTEGER     NOT NULL,"
  -- , "journal_entry_is_check BOOLEAN     NOT NULL,"
  , "PRIMARY KEY (journal_id)"
  , ")"
  ]

journalTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlTimestamptz
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  -- , Field SqlBool
  )
  ( Field SqlInt4
  , Field SqlTimestamptz
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  -- , Field SqlBool
  )
journalTable = table "journal" (
  p5
    ( tableField "journal_id"
    , tableField "journal_timestamp"
    , tableField "journal_user"
    , tableField "journal_action"
    , tableField "journal_total_amount"
    -- , tableField "journal_entry_is_check"
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
          , Maybe Int
          , Int
          , Int
          )
        ]
  return $ fst $ foldr
    (\(id_, ts, user, action, tot) (fin, before)->
      (JournalEntry id_ ts user (toEnum action) (tot - before) tot : fin, tot)
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
          , Maybe Int
          , Int
          , Int
          )
        ]
  if not (null lastTwoEntries)
  then do
    let diff = foldl (\acc (_, _, _, _, tot) -> tot - acc) 0 lastTwoEntries
        (jid, ts, user, action, total) = head lastTwoEntries
    return $ Just $ JournalEntry jid ts user (toEnum action) diff total
  else
    return Nothing


insertNewJournalEntry
  :: JournalSubmit
  -> PGS.Connection
  -> MateHandler Int
insertNewJournalEntry (JournalSubmit user action amount) conn = do
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
        , C.constant user
        , C.constant (fromEnum action)
        , C.constant (lastTotal + amount)
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }

insertNewCashCheck
  :: JournalCashCheck
  -> PGS.Connection
  -> MateHandler Int
insertNewCashCheck (JournalCashCheck user amount) conn =
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
        , C.constant (Just user)
        , C.constant (fromEnum CashCheck)
        , C.constant amount
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }
