{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Auth where

import Servant

import Control.Arrow ((<<<))

import Control.Monad (void)

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader (ask)

import Control.Concurrent (threadDelay)

import Control.Concurrent.STM

import Data.Profunctor.Product (p4)

import qualified Database.PostgreSQL.Simple as PGS

import Data.Int (Int64)

import qualified Data.Text as T
import Data.Text.Encoding

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
  , "token_string TEXT        NOT NULL PRIMARY KEY,"
  , "token_user   INTEGER     REFERENCES \"user\"(user_id) NOT NULL,"
  , "token_expiry TIMESTAMPTZ NOT NULL,"
  , "token_method INT         NOT NULL"
  , ")"
  ]

tokenTable :: Table
  ( Field SqlText
  , Field SqlInt4
  , Field SqlTimestamptz
  , Field SqlInt4
  )
  ( Field SqlText
  , Field SqlInt4
  , Field SqlTimestamptz
  , Field SqlInt4
  )
tokenTable = table "token" (
  p4
    ( tableField "token_string"
    , tableField "token_user"
    , tableField "token_expiry"
    , tableField "token_method"
    )
  )


initAuthData :: PGS.Query
initAuthData = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"auth_data\" ("
  , "auth_data_id      SERIAL  PRIMARY KEY,"
  , "auth_data_user    INTEGER NOT NULL REFERENCES \"user\"(\"user_id\") ON DELETE CASCADE,"
  , "auth_data_method  INTEGER NOT NULL,"
  , "auth_data_payload TEXT    NOT NULL"
  , ")"
  ]

authDataTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlText
  )
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlText
  )
authDataTable = table "auth_data" (
  p4
    ( tableField "auth_data_id"
    , tableField "auth_data_user"
    , tableField "auth_data_method"
    , tableField "auth_data_payload"
    )
  )


delayTime :: Int
delayTime = 1 * 10 ^ (6 :: Int)

getUserAuthInfo
  :: Int
  -> AuthMethod
  -> MateHandler AuthInfo
getUserAuthInfo uid method = do
  conn <- rsConnection <$> ask
  authdata <- liftIO $ do
    void $ threadDelay delayTime
    runSelect conn (
      keepWhen (\(_, duid, dmethod, _) ->
        duid .== C.constant uid .&& dmethod .== C.constant (fromEnum method))
          <<< queryTable authDataTable
      ) :: IO
          [ ( Int
            , Int
            , Int
            , T.Text
            )
          ]
  if null authdata
  then
    -- generate mock AuthInfo
    liftIO $ do
      rand1 <- decodeUtf8 <$> random 23
      rand2 <- case method of
        ChallengeResponse -> Just <$> decodeUtf8 <$> random 23
        _                 -> return Nothing
      return $ AuthInfo rand2 (AuthTicket rand1)
  else
    uncurry AuthInfo <$> newTicket uid method


validateToken
  :: ByteString
  -> PGS.Connection
  -> Handler (Maybe Int)
validateToken header conn = do
  tokens <- liftIO $ runSelect conn (
    keepWhen (\(tstr, _, _, _) ->
      tstr .== C.constant (decodeUtf8 header)) <<< queryTable tokenTable
    ) :: Handler
        [ ( T.Text
          , Int
          , UTCTime
          , Int
          )
        ]
  case tokens of
    [(_, uid, stamp, method)] -> do
      now <- liftIO $ getCurrentTime
      if diffUTCTime stamp now > 0
      then return $ Just uid
      else do
        void $ deleteToken (decodeUtf8 header) conn
        liftIO $ threadDelay delayTime
        throwError $ err401
          { errBody = "Your token expired!"
          }
    _ -> do
      liftIO $ threadDelay delayTime
      throwError $ err401
        { errBody = "No valid token found!"
        }


generateToken
  :: Ticket
  -> AuthResponse
  -> MateHandler AuthResult
generateToken (Ticket _ tuid _ (method, pl)) (AuthResponse response) = do
  conn <- rsConnection <$> ask
  authData <- liftIO $ runSelect conn (
    keepWhen (\(_, auid, amethod, _) ->
      auid .== C.constant tuid .&& amethod .== C.constant (fromEnum method))
        <<< queryTable authDataTable
    ) :: MateHandler
        [ ( Int
          , Int
          , Int
          , T.Text
          )
        ]
  let userPayloads = map (\(_, _, _, payload) -> payload) authData
      authResult = case method of
        PrimaryPass       -> validatePass response userPayloads
        SecondaryPass     -> validatePass response userPayloads
        ChallengeResponse -> validateChallengeResponse response userPayloads
  if authResult
  then do
    token <- liftIO $ Token
      <$> (decodeUtf8 <$> random 23)
      <*> pure tuid
      <*> (addUTCTime (23*60) <$> getCurrentTime)
      <*> pure method
    void $ insertToken token conn
    return $ Granted (AuthToken $ tokenString token)
  else
    return Denied
  where
    validatePass provided presents =
      any (\present -> provided == present) presents
    validateChallengeResponse provided presents =
      error "Validation of challnge response authentication not yet implemented"


insertToken
  :: Token
  -> PGS.Connection
  -> MateHandler T.Text
insertToken (Token tString tUser tExpiry tMethod) conn =
  fmap head $ liftIO $ runInsert_ conn $ Insert
    { iTable = tokenTable
    , iRows =
      [
      ( C.constant tString
      , C.constant tUser
      , C.constant tExpiry
      , C.constant (fromEnum tMethod)
      )
      ]
    , iReturning = rReturning (\(ident, _, _, _) -> ident)
    , iOnConflict = Nothing
    }


deleteToken
  :: T.Text
  -> PGS.Connection
  -> Handler Int64
deleteToken tstr conn =
  liftIO $ runDelete_ conn $ Delete
    { dTable     = tokenTable
    , dWhere     = (\(rtstr, _, _, _) -> rtstr .== C.constant tstr)
    , dReturning = rCount
    }


deleteTokenByUserId
  :: Int
  -> PGS.Connection
  -> MateHandler Int64
deleteTokenByUserId uid conn = liftIO $ runDelete_ conn $ Delete
  { dTable     = tokenTable
  , dWhere     = (\(_, rid, _, _) -> rid .== C.constant uid)
  , dReturning = rCount
  }


newTicket :: Int -> AuthMethod -> MateHandler (Maybe T.Text, AuthTicket)
newTicket ident method = do
  store <- rsTicketStore <$> ask
  rand1 <- liftIO $ (decodeUtf8 <$> random 23)
  rand2 <- liftIO $ case method of
    ChallengeResponse -> Just <$> (decodeUtf8 <$> random 23)
    _                 -> return Nothing
  later <- liftIO $ (addUTCTime 23 <$> getCurrentTime)
  let ticket = Ticket
        { ticketId     = AuthTicket rand1
        , ticketUser   = ident
        , ticketExpiry = later
        , ticketMethod = (method, rand2)
        }
  liftIO $ atomically $ modifyTVar store (\s -> S.insert ticket s)
  return (rand2, AuthTicket rand1)


processAuthRequest
  :: AuthRequest
  -> MateHandler AuthResult
processAuthRequest (AuthRequest aticket hash) = do
  store <- liftIO . readTVarIO =<< rsTicketStore <$> ask
  let mticket = S.filter (\st -> ticketId st == aticket) store
  case S.toList mticket of
    [ticket] -> do
      now <- liftIO $ getCurrentTime
      liftIO $ threadDelay delayTime
      if now > ticketExpiry ticket
      then
#if defined(DEVELOP)
        do
          mockticket <- Ticket <$>
            pure aticket <*>
            pure 1 <*>
            liftIO getCurrentTime <*>
            pure (PrimaryPass, Nothing)
          generateToken mockticket hash
#else
        return Denied
#endif
      else
        generateToken ticket hash
    _        -> do
      liftIO $ threadDelay delayTime
#if defined(DEVELOP)
      do
        mockticket <- Ticket <$>
          pure aticket <*>
          pure 1 <*>
          liftIO getCurrentTime <*>
          pure (PrimaryPass, Nothing)
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
