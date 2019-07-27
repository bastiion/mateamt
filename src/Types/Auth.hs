{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Auth where

import GHC.Generics

import Data.Aeson

import Data.ByteString
import qualified Data.ByteString.Base16 as B16

import qualified Data.Set as S

import Data.Time.Clock (UTCTime)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.IORef

import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM.TVar (TVar)

-- internal imports

data AuthInfo = AuthInfo
  { authSalt      :: AuthSalt
  , authAlgorithm :: AuthAlgorithm
  , authTicket    :: AuthTicket
  }
  deriving (Show, Generic)

instance ToJSON AuthInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AuthInfo


data AuthAlgorithm
  = SHA3_512
    deriving (Show, Read, Generic, Enum)

instance ToJSON AuthAlgorithm where
  toJSON = toJSON . show

instance FromJSON AuthAlgorithm where
  parseJSON j = read <$> parseJSON j


newtype AuthTicket = AuthTicket ByteString deriving (Show, Eq, Ord)

instance ToJSON AuthTicket where
  toJSON (AuthTicket bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthTicket where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthTicket enc)
      )


newtype AuthSalt = AuthSalt ByteString deriving (Show)

instance ToJSON AuthSalt where
  toJSON (AuthSalt bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthSalt where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthSalt enc)
      )


newtype AuthHash = AuthHash ByteString deriving (Show)

instance ToJSON AuthHash where
  toJSON (AuthHash bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthHash where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthHash enc)
      )


data AuthRequest = AuthRequest
  { authRequestTicket :: AuthTicket
  , authRequestHash   :: AuthHash
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


newtype AuthToken = AuthToken ByteString deriving (Show)

instance ToJSON AuthToken where
  toJSON (AuthToken bs) = (String . decodeUtf8 . B16.encode) bs

instance FromJSON AuthToken where
  parseJSON = withText ""
    (\t -> do
      let enc = fst $ B16.decode $ encodeUtf8 t
      return (AuthToken enc)
      )


data Token = Token
  { tokenString :: ByteString
  , tokenUser   :: Int
  , tokenExpiry :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON Token where
  toJSON (Token s _ _) = (String . decodeUtf8 . B16.encode) s


type TicketStore = TVar (S.Set Ticket)


data Ticket = Ticket
  { ticketId     :: AuthTicket
  , ticketUser   :: Int
  , ticketExpiry :: UTCTime
  }
  deriving (Ord)

instance Eq Ticket where
  (Ticket i1 _ _) == (Ticket i2 _ _) = i1 == i2
