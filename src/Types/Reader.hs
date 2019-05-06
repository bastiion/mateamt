module Types.Reader where

import Servant (Handler)

import Control.Monad.Reader (ReaderT)

import Database.PostgreSQL.Simple (Connection)

-- internal imports

import Types.Auth (TicketStore)

data ReadState = ReadState
  { rsConnection  :: Connection
  , rsTicketStore :: TicketStore
  }

type MateHandler = ReaderT ReadState Handler
