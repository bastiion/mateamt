{-# LANGUAGE OverloadedStrings #-}
module Control.Buy where

import Servant

import Control.Monad (void, foldM)

import Control.Monad.Reader (ask)

-- internal imports

import Types
import Model

buy
  :: Maybe Int
  -> [PurchaseDetail]
  -> MateHandler PurchaseResult
buy (Just auid) pds = do
  conn <- rsConnection <$> ask
  (missing, real) <- foldM (\acc@(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {purchaseDetailAmount = miss}):ms
        , (pd {purchaseDetailAmount = max 0 (purchaseDetailAmount pd - miss)}:rs)
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  void $ mapM_ (\pd -> postBuyProductAmountUpdate pd conn) real
  price <- foldM
    (\total pd ->
      fmap (+ total) (getLatestTotalPrice pd conn)
    )
    0
    real
  addToUserBalance auid (-price) conn
  newBalance <- userBalanceSelect conn auid
  return $ PurchaseResult
    ( if newBalance < 0
      then PurchaseDebtful
      else PurchaseOK
    )
    missing
buy Nothing pds = do
  conn <- rsConnection <$> ask
  (missing, real) <- foldM (\acc@(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {purchaseDetailAmount = miss}):ms
        , (pd {purchaseDetailAmount = max 0 (purchaseDetailAmount pd - miss)}:rs)
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  void $ mapM_
    (\pd -> postBuyProductAmountUpdate pd conn)
    real
  price <- foldM
    (\total pd ->
      fmap (+ total) (getLatestTotalPrice pd conn)
    )
    0
    real
  void $ insertNewJournalEntry (JournalSubmit "Cash purchase" price) conn
  return $ PurchaseResult
    (PayAmount price)
    missing
