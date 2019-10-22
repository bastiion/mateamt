{-# LANGUAGE FlexibleContexts #-}
module Util where

import Opaleye

import Data.Maybe (maybe)

import Data.Profunctor.Product.Default (Default)

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres
