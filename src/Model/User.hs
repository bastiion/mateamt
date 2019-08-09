{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Model.User where

import Data.Text as T hiding (head, foldl)
import Data.Time.Calendar
import Data.Time.Clock

import Data.Profunctor.Product (p9)

import Data.Maybe (fromJust, isJust, fromMaybe)

import Data.ByteString hiding (head, foldl)

import Data.Int (Int64)

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

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
  [ "create table if not exists \"user\" ("
  , "user_id serial primary key,"
  , "user_ident text not null,"
  , "user_balance integer not null,"
  , "user_timestamp date not null,"
  , "user_email text,"
  , "user_avatar integer,"
  , "user_salt bytea not null,"
  , "user_hash bytea,"
  , "user_algo integer"
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
  :: PGS.Connection
  -> Maybe Refine
  -> MateHandler [User]
userSelect conn ref = do
  today <- utctDay <$> (liftIO $ getCurrentTime)
  users <- liftIO $ runSelect conn (case ref of
      Nothing -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
        ts .>= C.constant (addDays (-30) today)
        ) <<< queryTable userTable
      Just All -> selectTable userTable
      Just Old -> keepWhen (\(_, _, _, ts, _, _, _, _, _) ->
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
      User i1 i2 i3 i4 i5 i6 (AuthSalt i7) (AuthHash <$> i8) (toEnum <$> i9)
      )
    users

userDetailsSelect
  :: PGS.Connection
  -> Int
  -> MateHandler UserDetails
userDetailsSelect conn id = do
  today <- utctDay <$> (liftIO $ getCurrentTime)
  users <- liftIO $ runSelect conn (
      keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
        uid .== C.constant id
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
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> return $
      UserDetails i2 i3 i5 i6 (AuthSalt i7) (toEnum <$> i9)
      )
    users


userBalanceSelect
  :: PGS.Connection
  -> Int
  -> MateHandler Int
userBalanceSelect conn id = do
  today <- utctDay <$> (liftIO $ getCurrentTime)
  users <- liftIO $ runSelect conn (
      keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
        uid .== C.constant id
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
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> return $
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
  , iReturning = rReturning (\(id, _, _, _, _, _, _, _, _) -> id)
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
  , uUpdateWith = updateEasy (\(id_, _, i3, _, _, _, i7, i8, _) ->
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
