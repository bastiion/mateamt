{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Types.Auth where

import GHC.Generics

import Data.Aeson

import qualified Data.Set as S

import Data.Time.Clock (UTCTime)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

import qualified Data.Text as T
import Data.Text.Encoding

import Control.Concurrent.STM.TVar (TVar)

-- internal imports

import Classes

data TicketRequest = TicketRequest
  { ticketRequestUser   :: Int
  , ticketRequestMethod :: AuthMethod
  }
  deriving (Show, Generic)

instance ToJSON TicketRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TicketRequest


data AuthInfo = AuthInfo
  { authChallenge :: Maybe T.Text
  , authTicket    :: AuthTicket
  }
  deriving (Show, Generic)

instance ToJSON AuthInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthInfo

-- instance ToDatabase AuthInfo where
-- 
--   type InTuple AuthInfo = (Maybe T.Text, T.Text)
-- 
--   toDatabase (AuthInfo mChallenge (AuthTicket ticket)) =
--     (mChallenge, ticket)

instance FromDatabase AuthInfo where

  type OutTuple AuthInfo = (Maybe T.Text, T.Text)

  fromDatabase (mChallenge, ticket) =
    AuthInfo mChallenge (AuthTicket ticket)


data AuthMethod
  = PrimaryPass
  | SecondaryPass
  | ChallengeResponse
  deriving (Show, Generic, Enum, Eq, Ord)

instance ToJSON AuthMethod where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthMethod


data AuthSubmit = AuthSubmit
  { authSubmitMethod  :: AuthMethod
  , authSubmitComment :: T.Text
  , authSubmitPayload :: T.Text
  }
  deriving (Show, Generic)

instance ToJSON AuthSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthSubmit


newtype AuthTicket = AuthTicket T.Text deriving (Show, Generic, Eq, Ord)

instance ToJSON AuthTicket where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthTicket


newtype AuthResponse = AuthResponse T.Text deriving (Show, Generic)

instance ToJSON AuthResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthResponse


data AuthRequest = AuthRequest
  { authRequestTicket :: AuthTicket
  , authRequestHash   :: AuthResponse
  }
  deriving (Show, Generic)

instance ToJSON AuthRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthRequest


data AuthResult
  = Granted
    { authToken :: AuthToken
    }
  | Denied
    deriving (Show, Generic)

instance ToJSON AuthResult where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthResult


newtype AuthToken = AuthToken T.Text deriving (Show, Generic)

instance ToJSON AuthToken where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthToken


data Token = Token
  { tokenString :: T.Text
  , tokenUser   :: Int
  , tokenExpiry :: UTCTime
  , tokenMethod :: AuthMethod
  }
  deriving (Generic, Show)

-- instance ToDatabase Token where
-- 
--   type InTuple Token = (T.Text, Int, UTCTime, Int)
-- 
--   toDatabase (Token string usr exp method) =
--     (string, usr, exp, fromEnum method)

instance FromDatabase Token where

  type OutTuple Token = (T.Text, Int, UTCTime, Int)

  fromDatabase (string, usr, exp, method) =
    Token string usr exp (toEnum method)


type TicketStore = TVar (S.Set Ticket)


data Ticket = Ticket
  { ticketId     :: AuthTicket
  , ticketUser   :: Int
  , ticketExpiry :: UTCTime
  , ticketMethod :: (AuthMethod, Maybe T.Text)
  }
  deriving (Show, Ord)

instance Eq Ticket where
  (Ticket i1 _ _ _) == (Ticket i2 _ _ _) = i1 == i2


data AuthData = AuthData
  { authDataId      :: Int
  , authDataUser    :: Int
  , authDataMethod  :: AuthMethod
  , authDataComment :: T.Text
  , authDataPayload :: T.Text
  }
  deriving (Show)

-- instance ToDatabase AuthData where
-- 
--   type InTuple AuthData = (Int, Int, Int, T.Text, ByteString)
-- 
--   toDatabase (AuthData id_ usr method comm payload) =
--     (id_, usr, fromEnum method, comm, (B64.decode $ encodeUtf8 payload))

instance FromDatabase AuthData where

  type OutTuple AuthData = (Int, Int, Int, T.Text, ByteString)

  fromDatabase (id_, usr, method, comm, payload) =
    AuthData id_ usr (toEnum method) comm (decodeUtf8 $ B64.encode payload)


data AuthOverview = AuthOverview
  { authOverviewId      :: Int
  , authOverviewComment :: T.Text
  , authOverviewMethod  :: AuthMethod
  }
  deriving (Show, Generic)

instance ToJSON AuthOverview where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthOverview

-- instance ToDatabase AuthOverview where
-- 
--   type InTuple AuthOverview = (Int, T.Text, Int)
-- 
--   toDatabase (AuthOverview id_ comm method) =
--     (id_, comm, fromEnum method)

instance FromDatabase AuthOverview where

  type OutTuple AuthOverview = (Int, T.Text, Int)

  fromDatabase (id_, comm, method) =
    AuthOverview id_ comm (toEnum method)
