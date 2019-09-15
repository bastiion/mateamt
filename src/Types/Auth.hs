{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Auth where

import GHC.Generics

import Data.Aeson

import qualified Data.Set as S

import Data.Time.Clock (UTCTime)

import qualified Data.Text as T

import Control.Concurrent.STM.TVar (TVar)

-- internal imports

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


data AuthMethod
  = PrimaryPass
  | SecondaryPass
  | ChallengeResponse
  deriving (Show, Generic, Enum, Eq, Ord)

instance ToJSON AuthMethod where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthMethod


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
  , authDataPayload :: T.Text
  }
  deriving (Show)
