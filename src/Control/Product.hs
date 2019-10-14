{-# LANGUAGE OverloadedStrings #-}
module Control.Product where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (asks)

import Data.Maybe (fromMaybe)

-- internal imports

import Types
import Model

productNew
  :: Maybe (Int, AuthMethod)
  -> ProductSubmit
  -> MateHandler Int
productNew (Just _) bevsub = do
  conn <- asks rsConnection
  bevid <- insertProduct bevsub conn
  void $ insertNewEmptyAmount bevid bevsub conn
  return bevid
productNew Nothing _ =
  throwError err401

productOverview
  :: Int
  -> MateHandler ProductOverview
productOverview pid = do
  conn <- asks rsConnection
  productOverviewSelectSingle pid conn

productStockRefill
  :: Maybe (Int, AuthMethod)
  -> [AmountRefill]
  -> MateHandler ()
productStockRefill (Just _) amorefs =
  if all ((>= 0) . amountRefillAmount) amorefs
  then do
    conn <- asks rsConnection
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
productStockUpdate (Just _) amoups =
  if all ((>= 0) . amountUpdateRealAmount) amoups
  then do
    conn <- asks rsConnection
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
  conn <- asks rsConnection
  productOverviewSelect (fromMaybe AvailableProducts mrefine) conn

productShortList
  :: Maybe ProductRefine
  -> MateHandler [ProductShortOverview]
productShortList mrefine = do
  conn <- asks rsConnection
  productShortOverviewSelect (fromMaybe AvailableProducts mrefine) conn
