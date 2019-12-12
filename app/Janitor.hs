module Janitor where

import qualified Database.PostgreSQL.Simple as PGS

import Control.Monad (void)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically, modifyTVar)

import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime)

-- internal imports

import Model.Auth
import Types.Auth

forkCleanProcess
  :: PGS.Connection
  -> TicketStore
  -> IO ()
forkCleanProcess conn store =
  void $ forkIO $ cleanProcess conn store

cleanProcess
  :: PGS.Connection
  -> TicketStore
  -> IO ()
cleanProcess conn store = do
  threadDelay $ 20 * 10 ^ (6 :: Int)
  cleanTokens conn
  cleanTickets store
  cleanProcess conn store

cleanTickets
  :: TicketStore
  -> IO ()
cleanTickets store = do
  now <- getCurrentTime
  atomically $
    modifyTVar store (S.filter (\(Ticket _ _ expiry _) -> expiry >= now))

cleanTokens
  :: PGS.Connection
  -> IO ()
cleanTokens conn = do
  now <- getCurrentTime
  void $ deleteOldTokens now conn
