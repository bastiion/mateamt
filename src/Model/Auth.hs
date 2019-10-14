{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model.Auth where

import Servant

import Control.Arrow

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Data.Profunctor.Product (p4, p5)

import qualified Database.PostgreSQL.Simple as PGS

import Data.Int (Int64)

import qualified Data.Text as T
import Data.Text.Encoding

import qualified Data.Set as S

import Data.Time.Clock

import Data.ByteString as B (ByteString)
import Data.ByteString.Random
import qualified Data.ByteString.Base64 as B64

import Opaleye hiding (null)
import qualified Opaleye.Constant as C

-- internal imports

import Types.Auth
import Types.Reader


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
  , "auth_data_comment TEXT    NOT NULL,"
  , "auth_data_payload BYTEA   NOT NULL"
  , ")"
  ]

authDataTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlText
  , Field SqlBytea
  )
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlText
  , Field SqlBytea
  )
authDataTable = table "auth_data" (
  p5
    ( tableField "auth_data_id"
    , tableField "auth_data_user"
    , tableField "auth_data_method"
    , tableField "auth_data_comment"
    , tableField "auth_data_payload"
    )
  )


delayTime :: Int
delayTime = 1 * 10 ^ (6 :: Int)


generateRandomText :: IO T.Text
generateRandomText = decodeUtf8 . B64.encode <$> random 23


selectAuthOverviews
  :: Int
  -> PGS.Connection
  -> MateHandler [AuthOverview]
selectAuthOverviews uid conn = do
  authData <- liftIO $ runSelect conn (proc () -> do
    (adid, aduid, admethod, adcomment, _) <-
      queryTable authDataTable -< ()
    restrict -< aduid .== C.constant uid
    returnA -< (adid, adcomment, admethod)
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          )
        ]
  return $ map
    (\(adid, adcomment, admethod) ->
      AuthOverview adid adcomment (toEnum admethod)
      )
    authData


getUserAuthInfo
  :: Int
  -> AuthMethod
  -> PGS.Connection
  -> MateHandler AuthInfo
getUserAuthInfo uid method conn = do
  authdata <- map (\(aid, auid, amethod, acomment, apayload) ->
    (aid, auid, amethod, acomment, decodeUtf8 $ B64.encode apayload)) <$>
    liftIO (do
      void $ threadDelay delayTime
      runSelect conn (proc () -> do
        (aid, auid, amethod, acomment, apayload) <-
          queryTable authDataTable -< ()
        restrict -<
          auid .== C.constant uid .&& amethod .== C.constant (fromEnum method)
        returnA -< (aid, auid, amethod, acomment, apayload)
        ) :: IO
            [ ( Int
              , Int
              , Int
              , T.Text
              , ByteString
              )
            ])
  if null authdata
  then
    -- generate mock AuthInfo
    liftIO $ do
      rand1 <- generateRandomText
      rand2 <- case method of
        ChallengeResponse -> Just <$> generateRandomText
        _                 -> return Nothing
      return $ AuthInfo rand2 (AuthTicket rand1)
  else
    uncurry AuthInfo <$> newTicket uid method


putUserAuthInfo
  :: Int
  -> AuthMethod
  -> T.Text
  -> T.Text
  -> PGS.Connection
  -> MateHandler Int
putUserAuthInfo uid method comment payload conn =
  fmap head $ liftIO $ runInsert_ conn $ Insert
    { iTable = authDataTable
    , iRows =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant uid
      , C.constant (fromEnum method)
      , C.constant comment
      , C.constant (encodeUtf8 payload)
      )
      ]
    , iReturning = rReturning (\(adid, _, _, _, _) -> adid)
    , iOnConflict = Nothing
    }


deleteAuthDataById
  :: Int
  -> PGS.Connection
  -> MateHandler Int64
deleteAuthDataById adid conn = liftIO $ runDelete_ conn $ Delete
  { dTable     = authDataTable
  , dWhere     = \(aid, _, _, _, _) -> aid .== C.constant adid
  , dReturning = rCount
  }


validateToken
  :: ByteString
  -> PGS.Connection
  -> Handler (Maybe (Int, AuthMethod))
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
      now <- liftIO getCurrentTime
      if diffUTCTime stamp now > 0
      then return $ Just (uid, toEnum method)
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
  -> PGS.Connection
  -> MateHandler AuthResult
generateToken (Ticket _ tuid _ (method, _)) (AuthResponse response) conn = do
  authData <- liftIO $ runSelect conn (
    keepWhen (\(_, auid, amethod, _, _) ->
      auid .== C.constant tuid .&& amethod .== C.constant (fromEnum method))
        <<< queryTable authDataTable
    ) :: MateHandler
        [ ( Int
          , Int
          , Int
          , T.Text
          , ByteString
          )
        ]
  let userPayloads = map
        (\(_, _, _, _, payload) ->
          decodeUtf8 payload
          )
        authData
      authResult = case method of
        PrimaryPass       -> validatePass response userPayloads
        SecondaryPass     -> validatePass response userPayloads
        ChallengeResponse -> validateChallengeResponse response userPayloads
  -- liftIO $ print (response : userPayloads)
  if authResult
  then do
    token <- liftIO $ Token
      <$> generateRandomText
      <*> pure tuid
      <*> (addUTCTime (23*60) <$> getCurrentTime)
      <*> pure method
    void $ insertToken token conn
    return $ Granted (AuthToken $ tokenString token)
  else
    return Denied
  where
    validatePass provided =
      any (provided ==)
    validateChallengeResponse _ _ =
      error "Validation of challenge response authentication not yet implemented"


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
    , dWhere     = \(rtstr, _, _, _) -> rtstr .== C.constant tstr
    , dReturning = rCount
    }


deleteTokenByUserId
  :: Int
  -> PGS.Connection
  -> MateHandler Int64
deleteTokenByUserId uid conn = liftIO $ runDelete_ conn $ Delete
  { dTable     = tokenTable
  , dWhere     = \(_, rid, _, _) -> rid .== C.constant uid
  , dReturning = rCount
  }


newTicket :: Int -> AuthMethod -> MateHandler (Maybe T.Text, AuthTicket)
newTicket ident method = do
  store <- rsTicketStore <$> ask
  rand1 <- liftIO generateRandomText
  rand2 <- liftIO $ case method of
    ChallengeResponse -> Just <$> generateRandomText
    _                 -> return Nothing
  later <- liftIO (addUTCTime 23 <$> getCurrentTime)
  let ticket = Ticket
        { ticketId     = AuthTicket rand1
        , ticketUser   = ident
        , ticketExpiry = later
        , ticketMethod = (method, rand2)
        }
  liftIO $ atomically $ modifyTVar store (S.insert ticket)
  return (rand2, AuthTicket rand1)


processAuthRequest
  :: AuthRequest
  -> S.Set Ticket
  -> PGS.Connection
  -> MateHandler AuthResult
processAuthRequest (AuthRequest aticket hash) store conn = do
  let mticket = S.filter (\st -> ticketId st == aticket) store
  case S.toList mticket of
    [ticket] -> do
      -- liftIO $ putStrLn "there is a ticket..."
      now <- liftIO getCurrentTime
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
          generateToken mockticket hash conn
#else
        return Denied
#endif
      else
        -- liftIO $ putStrLn "...and it is valid"
        generateToken ticket hash conn
    _        -> do
      liftIO $ threadDelay delayTime
#if defined(DEVELOP)
      do
        mockticket <- Ticket <$>
          pure aticket <*>
          pure 1 <*>
          liftIO getCurrentTime <*>
          pure (PrimaryPass, Nothing)
        generateToken mockticket hash conn
#else
      return Denied
#endif

processLogout
  :: Int
  -> PGS.Connection
  -> MateHandler ()
processLogout uid conn =
  void $ deleteTokenByUserId uid conn
