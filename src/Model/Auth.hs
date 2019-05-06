{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Auth where

import GHC.Generics

import Control.Arrow ((<<<))

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader (ask)

import Data.Profunctor.Product (p3)

import qualified Database.PostgreSQL.Simple as PGS

import Data.Text (Text)

import Data.Fixed (Fixed(..))

import Data.Time.Calendar (Day)
import Data.Time.Clock (secondsToNominalDiffTime)

import Data.ByteString (ByteString)
import Data.ByteString.Random

import Data.Maybe (fromMaybe)

import Opaleye
import qualified Opaleye.Constant as C

-- internal imports

import Types.Auth
import Types.Reader

import Model.User

initToken :: PGS.Query
initToken = "create table if not exists \"token\" (token_string bytea not null primary key, token_user integer not null, token_expiry timestamptz not null)"

tokenTable :: Table
  ( Field SqlBytea
  , Field SqlInt4
  , Field SqlTimestamptz
  )
  ( Field SqlBytea
  , Field SqlInt4
  , Field SqlTimestamptz
  )
tokenTable = table "token" (
    p3
      ( tableField "token_string"
      , tableField "token_user"
      , tableField "token_expiry"
      )
    )

getUserAuthInfo
  :: PGS.Connection
  -> Int
  -> MateHandler AuthInfo
getUserAuthInfo conn id = do
  users <- liftIO $ runSelect conn (
    keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
      uid .== C.constant id) <<< queryTable userTable
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
  head <$> mapM (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) ->
      AuthInfo (AuthSalt i7) (toEnum $ fromMaybe 0 i9) <$> newTicket id
      )
    users

newTicket :: Int -> MateHandler AuthTicket
newTicket id = do
  store <- rsTicketStore <$> ask
  rand <- liftIO $ random 23
  later <- liftIO $ (addUTCTime (secondsToNominalDiffTime (MkFixed 23)) <$> getCurrentTime)
  let ticket = AuthTicket
        { ticketId     = rand
        , ticketUser   = id
        , ticketExpiry = later
        }
  liftIO $ modifyTVar store (\s -> insert tocket store)
  retur ticket
