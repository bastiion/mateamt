{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Auth where

import Servant

import GHC.Generics

import Control.Arrow ((<<<))

import Control.Monad (void)

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader (ask)

import Control.Concurrent (threadDelay, forkIO)

import Control.Concurrent.STM

import Data.Profunctor.Product (p3)

import qualified Database.PostgreSQL.Simple as PGS

import Data.Int (Int64)

import Data.Text (Text)

import qualified Data.Set as S

import Data.Time.Calendar (Day)
import Data.Time.Clock

import Data.ByteString (ByteString)
import Data.ByteString.Random

import Data.Maybe (fromMaybe)

import Opaleye hiding (null)
import qualified Opaleye.Constant as C

-- internal imports

import Types.Auth
import Types.Reader

import Model.User


initToken :: PGS.Query
initToken = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"token\" ("
  , "token_string BYTEA       NOT NULL PRIMARY KEY,"
  , "token_user   INTEGER     REFERENCES \"user\"(user_id) NOT NULL,"
  , "token_expiry TIMESTAMPTZ NOT NULL"
  , ")"
  ]

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
  users <- liftIO $ do
    void $ threadDelay $ 1 * 10 ^ 6
    runSelect conn (
      keepWhen (\(uid, _, _, _, _, _, _, _, _) ->
        uid .== C.constant ident) <<< queryTable userTable
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
  if null users
  then throwError $ err404
    { errBody = "No such user"
    }
  else
    head <$> mapM (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) ->
        AuthInfo (AuthSalt i7) (toEnum $ fromMaybe 0 i9) <$> newTicket ident
        )
      users


validateToken
  :: PGS.Connection
  -> ByteString
  -> Handler (Maybe Int)
validateToken conn header = do
  tokens <- liftIO $ runSelect conn (
    keepWhen (\(tstr, _, _) ->
      tstr .== C.constant header) <<< queryTable tokenTable
    ) :: Handler
        [ ( ByteString
          , Int
          , UTCTime
          )
        ]
  case tokens of
    [(_, uid, stamp)] -> do
      now <- liftIO $ getCurrentTime
      if diffUTCTime stamp now > 0
      then return $ Just uid
      else do
        void $ deleteToken header conn
        liftIO $ threadDelay $ 1 * 10 ^ 6
        throwError $ err401
          { errBody = "Your token expired!"
          }
    _ -> do
      liftIO $ threadDelay $ 1 * 10 ^ 6
      throwError $ err401
        { errBody = "No valid token found!"
        }


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
    void $ insertToken token conn
    return $ Granted (AuthToken $ tokenString token)
  else
    return Denied


insertToken
  :: Token
  -> PGS.Connection
  -> MateHandler ByteString
insertToken (Token tString tUser tExpiry) conn =
  fmap head $ liftIO $ runInsert_ conn $ Insert
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


deleteToken
  :: ByteString
  -> PGS.Connection
  -> Handler Int64
deleteToken tstr conn =
  liftIO $ runDelete_ conn $ Delete
    { dTable     = tokenTable
    , dWhere     = (\(rtstr, _, _) -> rtstr .== C.constant tstr)
    , dReturning = rCount
    }


deleteTokenByUserId
  :: Int
  -> PGS.Connection
  -> MateHandler Int64
deleteTokenByUserId uid conn = liftIO $ runDelete_ conn $ Delete
  { dTable     = tokenTable
  , dWhere     = (\(_, rid, _) -> rid .== C.constant uid)
  , dReturning = rCount
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
      liftIO $ threadDelay $ 1 * 10 ^ 6
      if now > ticketExpiry ticket
      then
#if defined(DEVELOP)
        do
          mockticket <- Ticket <$> pure aticket <*> pure 1 <*> liftIO getCurrentTime
          generateToken mockticket hash
#else
        return Denied
#endif
      else
        generateToken ticket hash
    _        -> do
      liftIO $ threadDelay $ 1 * 10 ^ 6
#if defined(DEVELOP)
      do
        mockticket <- Ticket <$> pure aticket <*> pure 1 <*> liftIO getCurrentTime
        generateToken mockticket hash
#else
      return Denied
#endif

processLogout
  :: Int
  -> MateHandler ()
processLogout uid = do
  conn <- rsConnection <$> ask
  void $ deleteTokenByUserId uid conn
