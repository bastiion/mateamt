{-# LANGUAGE FlexibleContexts #-}
module Util where

import Opaleye

import Data.Maybe (fromMaybe)

import Data.Profunctor.Product.Default (Default)

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . showSqlForPostgres
