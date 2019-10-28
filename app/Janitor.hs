{-# LANGUAGE OverloadedStrings #-}
module Janitor where

import qualified Data.Set as S

forkCleanProcess :: IO ()
forkCleanProcess =
  return()

cleanProcess conn store =
  return ()

cleanStore store =
  now <- getCurrentTime
  atomically $
    modifyTVar store (S.filter (\(Ticket _ _ exp _) -> exp >= now))
