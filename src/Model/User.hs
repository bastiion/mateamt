{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Model.User where

import Data.Text as T hiding (head)
import Data.Time.Calendar
import Data.Time.Clock

import Data.Profunctor.Product (p9)

import Data.Maybe (fromJust, isJust, fromMaybe)

import Data.ByteString hiding (head)
-- import Data.ByteString.Random

import Data.Int (Int64)

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

import Control.Arrow ((<<<))

import Opaleye as O
import qualified Opaleye.Constant as C

-- internal imports

import Types.User
import Types.Refine
import Types.Auth

initUser :: PGS.Query
initUser = "create table if not exists \"user\" (user_id serial primary key, user_ident varchar(128) not null, user_balance integer not null, user_timestamp date not null, user_email varchar(128), user_avatar integer, user_salt bytea not null, user_hash bytea, user_algo integer)"

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
  :: PGS.Connection
  -> Maybe Refine
  -> Bool
  -> IO [User]
userSelect conn ref sw = do
  today <- utctDay <$> getCurrentTime
  users <- runSelect conn (case ref of
      Nothing -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
        ts .>= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      Just All -> selectTable userTable
      Just Old -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
        ts .<= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      ) :: IO
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
      if sw
      then User i1 i2 i3 i4 i5 i6 (AuthSalt i7) (AuthHash <$> i8) (toEnum <$> i9)
      else QueryUser i1 i2 i6
      )
    users

insertUser :: UserSubmit -> Day -> ByteString -> Insert [Int]
insertUser us now randSalt = Insert
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
  , iReturning = rReturning (\(id, _, _, _, _, _, _, _, _) -> id)
  , iOnConflict = Nothing
  }

updateUser :: Int -> UserSubmit -> Day -> Update Int64
updateUser id us now = Update
  { uTable = userTable
  , uUpdateWith = updateEasy (\(id_, _, i3, _, _, i6, i7, i8, i9) ->
      ( id_
      , C.constant (userSubmitIdent us)
      , i3
      , C.constant (now)
      , C.constant (userSubmitEmail us)
      , i6
      , i7
      , i8
      , i9
      )
    )
  , uWhere = (\(i1, _, _, _, _, _, _, _, _) -> i1 .== C.constant id)
  , uReturning = rCount
  }
