{-# LANGUAGE OverloadedStrings #-}
module Control.Product where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (ask)

import Data.Maybe (fromMaybe)

-- internal imports

import Types
import Model

productNew
  :: Maybe (Int, AuthMethod)
  -> ProductSubmit
  -> MateHandler Int
productNew (Just _) bevsub = do
  conn <- rsConnection <$> ask
  bevid <- insertProduct bevsub conn
  void $ insertNewEmptyAmount bevid bevsub conn
  return bevid
productNew Nothing _ =
  throwError $ err401

productOverview
  :: Int
  -> MateHandler ProductOverview
productOverview pid = do
  conn <- rsConnection <$> ask
  productOverviewSelectSingle pid conn

productStockRefill
  :: Maybe (Int, AuthMethod)
  -> [AmountRefill]
  -> MateHandler ()
productStockRefill (Just _) amorefs = do
  if all ((>= 0) . amountRefillAmount) amorefs
  then do
    conn <- rsConnection <$> ask
    void $ manualProductAmountRefill amorefs conn
  else
    throwError $ err400
      { errBody = "Amounts less than 0 are not acceptable."
      }
productStockRefill Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

productStockUpdate
  :: Maybe (Int, AuthMethod)
  -> [AmountUpdate]
  -> MateHandler ()
productStockUpdate (Just _) amoups = do
  if all ((>= 0) . amountUpdateRealAmount) amoups
  then do
    conn <- rsConnection <$> ask
    void $ manualProductAmountUpdate amoups conn
  else
    throwError $ err400
      { errBody = "Amounts less than 0 are not acceptable."
      }
productStockUpdate Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

productList
  :: Maybe ProductRefine
  -> MateHandler [ProductOverview]
productList mrefine = do
  conn <- rsConnection <$> ask
  productOverviewSelect (fromMaybe AvailableProducts mrefine) conn

productShortList
  :: Maybe ProductRefine
  -> MateHandler [ProductShortOverview]
productShortList mrefine = do
  conn <- rsConnection <$> ask
  productShortOverviewSelect (fromMaybe AvailableProducts mrefine) conn
