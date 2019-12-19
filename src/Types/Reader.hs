module Types.Reader where

import qualified Data.Text as T

import Servant (Handler)

import Control.Monad.Reader (ReaderT)

import Database.PostgreSQL.Simple (Connection)

-- internal imports

import Types.Auth (TicketStore)

data ReadState = ReadState
  { rsConnection      :: Connection
  , rsTicketStore     :: TicketStore
  , rsCurrencySymbol  :: T.Text
  , rsSoftwareVersion :: T.Text
  }

type MateHandler = ReaderT ReadState Handler
