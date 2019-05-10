{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Auth where

import GHC.Generics

import Control.Arrow ((<<<))

import Control.Monad (void)

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader (ask)

import Control.Concurrent.STM

import Data.Profunctor.Product (p3)

import qualified Database.PostgreSQL.Simple as PGS

import Data.Text (Text)

import qualified Data.Set as S

import Data.Time.Calendar (Day)
import Data.Time.Clock

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
  :: Int
  -> MateHandler AuthInfo
getUserAuthInfo ident = do
  conn <- rsConnection <$> ask
  users <- liftIO $ runSelect conn (
    keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
      uid .== C.constant ident) <<< queryTable userTable
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
  head <$> mapM (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) ->
      AuthInfo (AuthSalt i7) (toEnum $ fromMaybe 0 i9) <$> newTicket ident
      )
    users

generateToken
  :: Ticket
  -> AuthHash
  -> MateHandler AuthResult
generateToken (Ticket _ ident exp) (AuthHash hash) = do
  conn <- rsConnection <$> ask
  users <- liftIO $ runSelect conn (
    keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
      uid .== C.constant ident) <<< queryTable userTable
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
  let userHash = head $ map (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> i8) users
  if userHash == Nothing || userHash == Just hash
  then do
    token <- liftIO $ Token
      <$> (random 23)
      <*> (pure ident)
      <*> (addUTCTime (23*60) <$> getCurrentTime)
    void $ liftIO $ runInsert_ conn (insertToken token)
    return $ Granted (AuthToken $ tokenString token)
  else
    return Denied

insertToken
  :: Token
  -> Insert [ByteString]
insertToken (Token tString tUser tExpiry) = Insert
  { iTable = tokenTable
  , iRows =
    [
    ( C.constant tString
    , C.constant tUser
    , C.constant tExpiry
    )
    ]
  , iReturning = rReturning (\(ident, _, _) -> ident)
  , iOnConflict = Nothing
  }

newTicket :: Int -> MateHandler AuthTicket
newTicket ident = do
  store <- rsTicketStore <$> ask
  rand <- liftIO $ random 23
  later <- liftIO $ (addUTCTime 23 <$> getCurrentTime)
  let ticket = Ticket
        { ticketId     = AuthTicket rand
        , ticketUser   = ident
        , ticketExpiry = later
        }
  liftIO $ atomically $ modifyTVar store (\s -> S.insert ticket s)
  return (AuthTicket rand)

processAuthRequest
  :: AuthRequest
  -> MateHandler AuthResult
processAuthRequest (AuthRequest aticket hash) = do
  store <- liftIO . readTVarIO =<< rsTicketStore <$> ask
  let mticket = S.filter (\st -> ticketId st == aticket) store
  case S.toList mticket of
    [ticket] -> do
      now <- liftIO $ getCurrentTime
      if now > ticketExpiry ticket
      then
        return Denied
      else
        generateToken ticket hash
    _        -> return Denied
