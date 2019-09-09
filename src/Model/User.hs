{-# LANGUAGE OverloadedStrings #-}
module Model.User where

import Data.Text as T hiding (head, foldl)
import Data.Time.Calendar
import Data.Time.Clock

import Data.Profunctor.Product (p9)

import Data.ByteString hiding (head, foldl)

import Data.Int (Int64)

import qualified Database.PostgreSQL.Simple as PGS

import Control.Arrow ((<<<))

import Control.Monad.IO.Class (liftIO)

import Opaleye as O
import qualified Opaleye.Constant as C

-- internal imports

import Types.User
import Types.Refine
import Types.Auth
import Types.Reader

initUser :: PGS.Query
initUser = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"user\" ("
  , "user_id        SERIAL PRIMARY KEY,"
  , "user_ident     TEXT NOT NULL,"
  , "user_balance   INTEGER NOT NULL,"
  , "user_timestamp DATE NOT NULL,"
  , "user_email     TEXT,"
  , "user_avatar    INTEGER REFERENCES \"avatar\"(\"avatar_id\") ON DELETE CASCADE,"
  , "user_salt      BYTEA NOT NULL,"
  , "user_hash      BYTEA,"
  , "user_algo      INTEGER"
  , ")"
  ]

userTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlInt4
  , Field SqlDate
  , FieldNullable SqlText
  , FieldNullable SqlInt4
  , Field SqlBytea
  , FieldNullable SqlBytea
  , FieldNullable SqlInt4
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlDate
  , FieldNullable SqlText
  , FieldNullable SqlInt4
  , Field SqlBytea
  , FieldNullable SqlBytea
  , FieldNullable SqlInt4
  )
userTable = table "user" (
  p9
    ( tableField "user_id"
    , tableField "user_ident"
    , tableField "user_balance"
    , tableField "user_timestamp"
    , tableField "user_email"
    , tableField "user_avatar"
    , tableField "user_salt"
    , tableField "user_hash"
    , tableField "user_algo"
    )
  )

userSelect
  :: Maybe UserRefine
  -> PGS.Connection
  -> MateHandler [UserSummary]
userSelect ref conn = do
  today <- utctDay <$> (liftIO $ getCurrentTime)
  users <- liftIO $ runSelect conn (case ref of
      Nothing -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
        ts .>= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      Just AllUsers -> selectTable userTable
      Just OldUsers -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
        ts .<= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      ) :: MateHandler
          [ ( Int
            , Text
            , Int
            , Day
            , Maybe Text
            , Maybe Int
            , ByteString
            , Maybe ByteString
            , Maybe Int
            )
          ]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> return $
      -- User i1 i2 i3 i4 i5 i6 (AuthSalt i7) (AuthHash <$> i8) (toEnum <$> i9)
      UserSummary i1 i2 i6
      )
    users

userDetailsSelect
  :: Int
  -> PGS.Connection
  -> MateHandler UserDetails
userDetailsSelect uid conn = do
  users <- liftIO $ runSelect conn (
      keepWhen (\(uuid, _, _, _, _, _, _, _, _) ->
        uuid .== C.constant uid
        ) <<< queryTable userTable
      ) :: MateHandler
          [ ( Int
            , Text
            , Int
            , Day
            , Maybe Text
            , Maybe Int
            , ByteString
            , Maybe ByteString
            , Maybe Int
            )
          ]
  head <$> mapM
    (\(i1, i2, i3, _, i5, i6, i7, _, i9) -> return $
      UserDetails i1 i2 i3 i5 i6 (AuthSalt i7) (toEnum <$> i9)
      )
    users


userBalanceSelect
  :: PGS.Connection
  -> Int
  -> MateHandler Int
userBalanceSelect conn uid = do
  users <- liftIO $ runSelect conn (
      keepWhen (\(uuid, _, _, _, _, _, _, _, _) ->
        uuid .== C.constant uid
        ) <<< queryTable userTable
      ) :: MateHandler
          [ ( Int
            , Text
            , Int
            , Day
            , Maybe Text
            , Maybe Int
            , ByteString
            , Maybe ByteString
            , Maybe Int
            )
          ]
  head <$> mapM
    (\(_, _, i3, _, _, _, _, _, _) -> return $
      i3
      )
    users


insertUser
  :: UserSubmit
  -> Day
  -> ByteString
  -> PGS.Connection
  -> MateHandler Int
insertUser us now randSalt conn = fmap head $ liftIO $ runInsert_ conn $ Insert
  { iTable = userTable
  , iRows  =
    [
    ( C.constant (Nothing :: Maybe Int)
    , C.constant (userSubmitIdent us)
    , C.constant (0 :: Int)
    , C.constant now
    , C.constant (userSubmitEmail us)
    , C.constant (Nothing :: Maybe Int)
    , C.constant randSalt
    , C.constant (Nothing :: Maybe ByteString)
    , C.constant (Nothing :: Maybe Int)
    )
    ]
  , iReturning = rReturning (\(uid, _, _, _, _, _, _, _, _) -> uid)
  , iOnConflict = Nothing
  }

updateUserDetails
  :: Int
  -> UserDetailsSubmit
  -> Day
  -> PGS.Connection
  -> MateHandler Int64
updateUserDetails uid uds now conn = liftIO $ runUpdate_ conn $ Update
  { uTable      = userTable
  , uUpdateWith = updateEasy (\(id_, _, i3, _, _, _, i7, _, _) ->
      ( id_
      , C.constant (userDetailsSubmitIdent uds)
      , i3
      , C.constant now
      , C.constant (userDetailsSubmitEmail uds)
      , C.constant (userDetailsSubmitAvatar uds)
      , i7
      , C.constant ((\(AuthHash h) -> h) <$> userDetailsSubmitHash uds)
      , C.constant (fromEnum <$> userDetailsSubmitAlgo uds)
      )
    )
  , uWhere      = (\(i1, _, _, _, _, _, _, _, _) -> i1 .== C.constant uid)
  , uReturning  = rCount
  }

addToUserBalance
  :: Int
  -> Int
  -> PGS.Connection
  -> MateHandler Int64
addToUserBalance uid amount conn = liftIO $ runUpdate_ conn $ Update
  { uTable      = userTable
  , uUpdateWith = updateEasy (\(id_, i2, i3, i4, i5, i6, i7, i8, i9) ->
      ( id_
      , i2
      , i3 + C.constant amount
      , i4
      , i5
      , i6
      , i7
      , i8
      , i9
      )
    )
  , uWhere      = (\(i1, _, _, _, _, _, _, _, _) -> i1 .== C.constant uid)
  , uReturning  = rCount
  }
