{-# LANGUAGE OverloadedStrings #-}
module Control.Buy where

import Control.Monad (void, foldM)

import Control.Monad.Reader (asks)

-- internal imports

import Types
import Model

buy
  :: Maybe (Int, AuthMethod)
  -> [PurchaseDetail]
  -> MateHandler PurchaseResult
buy (Just (auid, _)) pds = do
  conn <- asks rsConnection
  (missing, real) <- foldM (\(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {purchaseDetailAmount = miss}):ms
        , pd {purchaseDetailAmount = max 0 (purchaseDetailAmount pd - miss)}:rs
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  mapM_ (`postBuyProductAmountUpdate` conn) real
  price <- foldM
    (\total pd ->
      fmap (+ total) (getLatestTotalPrice pd conn)
    )
    0
    real
  void $ addToUserBalance auid (-price) conn
  newBalance <- userBalanceSelect conn auid
  return $ PurchaseResult
    ( if newBalance < 0
      then PurchaseDebtful
      else PurchaseOK
    )
    missing
buy Nothing pds = do
  conn <- asks rsConnection
  (missing, real) <- foldM (\(ms, rs) pd -> do
    mmiss <- checkProductAvailability pd conn
    case mmiss of
      Just miss -> return
        ( (pd {purchaseDetailAmount = miss}):ms
        , pd {purchaseDetailAmount = max 0 (purchaseDetailAmount pd - miss)}:rs
        )
      Nothing -> return
        ( ms
        , pd:rs
        )
    )
    ([], [])
    pds
  mapM_
    (`postBuyProductAmountUpdate` conn)
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
