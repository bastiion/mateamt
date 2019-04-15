{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Types

data User = User
  { userId        :: Int
  , userIdent     :: T.Text
  , userBalance   :: Int
  , userTimeStamp :: Day
  , userEmail     :: Maybe T.Text
  , userAvatar    :: Maybe Int
  , userPin       :: Maybe T.Text
  }
    deriving (Generic, Show)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

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
  -> IO [User]
userSelect conn ref = do
  today <- utctDay <$> getCurrentTime
  (mapM
    (\(i1, i2, i3, i4, i5, i6, i7) -> return $
      User
        i1
        i2
        i3
        i4
        i5
        i6
        i7
      )
    ) =<< runSelect conn (case ref of
      Nothing -> keepWhen (\(_, _, _, ts, _, _, _) ->
        ts .>= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      Just All -> selectTable userTable
      Just Old -> keepWhen (\(_, _, _, ts, _, _, _) ->
        ts .<= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      )

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
