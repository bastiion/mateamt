{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Model.User where

import Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Profunctor.Product (p7)

import Data.Aeson

import Data.Int (Int64)

import Data.Maybe (fromJust)

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

import Control.Arrow ((<<<))

import Opaleye as O
import qualified Opaleye.Constant as C

-- internal imports

import Types.Refine

data User
  = User
    { userId        :: Int
    , userIdent     :: T.Text
    , userBalance   :: Int
    , userTimeStamp :: Day
    , userEmail     :: Maybe T.Text
    , userAvatar    :: Maybe Int
    , userPin       :: Maybe T.Text
    }
  | QueryUser
    { userId        :: Int
    , userIdent     :: T.Text
    , userAvatar    :: Maybe Int
    }
  deriving (Generic, Show)

instance ToJSON User where
  toEncoding (User id ident balance ts email avatar _) =
    pairs
      (  "userId" .= id
      <> "userIdent" .= ident
      <> "userBalance" .= balance
      <> "userTimeStamp" .= ts
      <> "userEmail" .= email
      <> "userAvatar" .= avatar
      )
  toEncoding (QueryUser id ident avatar) =
    pairs
      (  "userId" .= id
      <> "userIdent" .= ident
      <> "userAvatar" .= avatar
      )

instance FromJSON User

data UserSubmit = UserSubmit
  { userSubmitIdent :: T.Text
  , userSubmitEmail :: Maybe T.Text
  , userSubmitPin   :: Maybe T.Text
  }
  deriving (Generic, Show)

instance ToJSON UserSubmit where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON UserSubmit

initUser :: PGS.Query
initUser = "create table if not exists \"user\" (id serial primary key, ident varchar(128) not null, balance integer not null, time_stamp date not null, email varchar(128), avatar integer, pin varchar(128))"

userTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlInt4
  , Field SqlDate
  , FieldNullable SqlText
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlDate
  , FieldNullable SqlText
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
userTable = table "user" (
  p7
    ( tableField "id"
    , tableField "ident"
    , tableField "balance"
    , tableField "time_stamp"
    , tableField "email"
    , tableField "avatar"
    , tableField "pin"
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
      Nothing -> keepWhen (\(_, _, _, ts, _, _, _) ->
        ts .>= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      Just All -> selectTable userTable
      Just Old -> keepWhen (\(_, _, _, ts, _, _, _) ->
        ts .<= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      ) :: IO [(Int, Text, Int, Day, Maybe Text, Maybe Int, Maybe Text)]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7) -> return $
      if sw
      then User i1 i2 i3 i4 i5 i6 i7
      else QueryUser i1 i2 i6
      )
    users

insertUser :: UserSubmit -> Day -> Insert [Int]
insertUser us now = Insert
  { iTable = userTable
  , iRows  =
    [
    ( C.constant (Nothing :: Maybe Int)
    , C.constant (userSubmitIdent us)
    , C.constant (0 :: Int)
    , C.constant now
    , C.constant (userSubmitEmail us)
    , C.constant (Nothing :: Maybe Int)
    , C.constant (userSubmitPin us)
    )
    ]
  , iReturning = rReturning (\(id, _, _, _, _, _, _) -> id)
  , iOnConflict = Nothing
  }

updateUser :: Int -> UserSubmit -> Day -> Update Int64
updateUser id us now = Update
  { uTable = userTable
  , uUpdateWith = updateEasy (\(id_, _, i3, _, _, i6, _) ->
      ( id_
      , C.constant (userSubmitIdent us)
      , i3
      , C.constant (now)
      , C.constant (userSubmitEmail us)
      , i6
      , C.constant (userSubmitPin us)
      )
    )
  , uWhere = (\(i1, _, _, _, _, _, _) -> i1 .== C.constant id)
  , uReturning = rCount
  }
