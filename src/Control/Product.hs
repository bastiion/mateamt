{-# LANGUAGE OverloadedStrings #-}
module Control.Product where

import Servant

import Control.Monad (void)

import Control.Monad.Reader (asks)

import Control.Monad.Extra (anyM, allM)

import Data.Maybe (fromMaybe, isJust)

-- internal imports

import Types
import Model
import Control.Role (checkCapability)

productNew
  :: Maybe (Int, AuthMethod)
  -> ProductSubmit
  -> MateHandler Int
productNew (Just (uid, auth)) bevsub = do
  mayAddProduct <- checkCapability uid roleCanManageProducts
  if auth `elem` [PrimaryPass, ChallengeResponse] && mayAddProduct
  then do
    conn <- asks rsConnection
    bevid <- insertProduct bevsub conn
    void $ insertNewEmptyAmount bevid bevsub conn
    return bevid
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
productNew Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

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
productStockRefill (Just (uid, auth)) amorefs = do
  mayRefill <- anyM
    (checkCapability uid)
    [ roleCanRefillStock, roleCanManageProducts ]
  if auth `elem` [PrimaryPass, ChallengeResponse] && mayRefill
  then do
    conn <- asks rsConnection
    let prods = map
          (\refill -> productSelectSingle (amountRefillProductId refill) conn)
          amorefs
    allProdsExist <- allM (fmap isJust) prods
    if allProdsExist
    then
      if
        all
          (\refill ->
            (>= 0) (amountRefillAmountSingles refill) &&
            (>= 0) (amountRefillAmountCrates refill)
            )
          amorefs
      then do
        void $ manualProductAmountRefill amorefs conn
      else
        throwError $ err400
          { errBody = "Amounts less than 0 are not acceptable."
          }
    else
      throwError $ err400
        { errBody = "Non-existent Products are non-refillable."
        }
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
productStockRefill Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

productStockUpdate
  :: Maybe (Int, AuthMethod)
  -> [AmountUpdate]
  -> MateHandler ()
productStockUpdate (Just (uid, method)) amoups = do
  mayUpdateStock <- checkCapability uid roleCanManageProducts
  if method `elem` [PrimaryPass, ChallengeResponse] && mayUpdateStock
  then
    if all ((>= 0) . amountUpdateRealAmount) amoups
    then do
      conn <- asks rsConnection
      void $ manualProductAmountUpdate amoups conn
    else
      throwError $ err400
        { errBody = "Amounts less than 0 are not acceptable."
        }
  else
    throwError $ err401
      { errBody = "Wrong Authentication present."
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
