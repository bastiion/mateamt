{-# LANGUAGE OverloadedStrings #-}
module Model.Amount where

import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime)

import Data.Profunctor.Product (p5)

import qualified Database.PostgreSQL.Simple as PGS

import Control.Arrow ((<<<))

import Control.Monad.IO.Class (liftIO)

import Opaleye as O
import Opaleye.Constant as C

-- internal imports

import Types

initAmount :: PGS.Query
initAmount = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"amount\" ("
  , "amount_product_id BIGINT      NOT NULL REFERENCES \"product\"(\"product_id\") ON DELETE CASCADE,"
  , "amount_timestamp  TIMESTAMPTZ NOT NULL,"
  , "amount_amount     INTEGER     NOT NULL,"
  , "amount_price      INTEGER     NOT NULL,"
  , "amount_verified   BOOLEAN     NOT NULL,"
  , "PRIMARY KEY (amount_product_id, amount_timestamp)"
  , ")"
  ]

amountTable :: Table
  ( Field SqlInt4
  , Field SqlTimestamptz
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlBool
  )
  ( Field SqlInt4
  , Field SqlTimestamptz
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlBool
  )
amountTable = table "amount" (
  p5
    ( tableField "amount_product_id"
    , tableField "amount_timestamp"
    , tableField "amount_amount"
    , tableField "amount_price"
    , tableField "amount_verified"
    )
  )

insertNewEmptyAmount
  :: Int -- ^ the associated product id
  -> ProductSubmit -- ^ submitted product data
  -> PGS.Connection
  -> MateHandler Int
insertNewEmptyAmount bevid (ProductSubmit _ price _ _ _ _ _ _ _) conn =
  liftIO $ do
    now <- getCurrentTime
    fmap head $ runInsert_ conn $ Insert
      { iTable = amountTable
      , iRows  =
        [
        ( C.constant bevid
        , C.constant now
        , C.constant (0 :: Int)
        , C.constant price
        , C.constant False
        )
        ]
      , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
      , iOnConflict = Nothing
      }

getLatestPriceByProductId
  :: Int             -- The associated Product ID
  -> PGS.Connection
  -> MateHandler Int -- The price in cents
getLatestPriceByProductId pid conn = do
  amounts <- liftIO $ runSelect conn $
    limit 1 $ orderBy (desc (\(_, ts, _, _, _) -> ts))
      (keepWhen (\(id_, _, _, _, _) -> id_ .== C.constant pid) <<< queryTable amountTable)
    :: MateHandler
      [ ( Int
        , UTCTime
        , Int
        , Int
        , Bool
        )
      ]
  head <$> mapM
    (\(_, _, _, price, _) -> return price)
    amounts

getLatestAmountByProductId
  :: Int             -- The associated Product ID
  -> PGS.Connection
  -> MateHandler Int -- The amount
getLatestAmountByProductId pid conn = do
  amounts <- liftIO $ runSelect conn $
    limit 1 $ orderBy (desc (\(_, ts, _, _, _) -> ts))
      (keepWhen (\(id_, _, _, _, _) -> id_ .== C.constant pid) <<< queryTable amountTable)
    :: MateHandler
      [ ( Int
        , UTCTime
        , Int
        , Int
        , Bool
        )
      ]
  return (head $ map
    (\(_, _, amount, _, _) -> amount)
    amounts
    )

getLatestTotalPrice
  :: PurchaseDetail  -- The associated PurchaseDetail
  -> PGS.Connection
  -> MateHandler Int -- The price in cents
getLatestTotalPrice (PurchaseDetail pid amount) conn = do
  amounts <- liftIO $ runSelect conn $
    limit 1 $ orderBy (desc (\(_, ts, _, _, _) -> ts)) $
      keepWhen (\(id_, _, _, _, _) -> id_ .== C.constant pid) <<<
        queryTable amountTable
    :: MateHandler
      [ ( Int
        , UTCTime
        , Int
        , Int
        , Bool
        )
      ]
  return $ ((amount *) . head) (map
    (\(_, _, _, price, _) -> price)
    amounts
    )

checkProductAvailability
  :: PurchaseDetail
  -> PGS.Connection
  -> MateHandler (Maybe Int) -- ^ Returns maybe missing amount
checkProductAvailability (PurchaseDetail pid amount) conn = do
  realamount <- (\(_, _, ramount, _, _) -> ramount) . head <$>
    (liftIO $ runSelect conn $ limit 1 $
      orderBy (desc (\(_, ts, _, _, _) -> ts)) $
        keepWhen (\(id_, _, _, _, _) -> id_ .== C.constant pid) <<<
          queryTable amountTable
      :: MateHandler
        [ ( Int
          , UTCTime
          , Int
          , Int
          , Bool
          )
        ]
    )
  if realamount < amount
  then return (Just $ amount - realamount)
  else return Nothing


manualProductAmountUpdate
  :: [AmountUpdate]
  -> PGS.Connection
  -> MateHandler [Int]
manualProductAmountUpdate aups conn =
  mapM
    (\(AmountUpdate pid amount) -> do
      oldprice <- getLatestPriceByProductId pid conn
      head <$> liftIO (do
        now <- getCurrentTime
        runInsert_ conn $ Insert
          { iTable = amountTable
          , iRows  =
            [
            ( C.constant pid
            , C.constant now
            , C.constant amount
            , C.constant oldprice
            , C.constant True
            )
            ]
          , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
          , iOnConflict = Nothing
          }
        )
      )
    aups


postBuyProductAmountUpdate
  :: PurchaseDetail
  -> PGS.Connection
  -> MateHandler Int
postBuyProductAmountUpdate (PurchaseDetail pid pdamount) conn = do
  now <- liftIO getCurrentTime
  (amount, oldprice) <- (\(_, _, am, op, _) -> (am, op)) . head <$> (
    liftIO $ runSelect conn $ limit 1 $
      orderBy (desc (\(_, ts, _, _, _) -> ts)) $
        keepWhen (\(id_, _, _, _, _) -> id_ .== C.constant pid) <<<
          queryTable amountTable
      :: MateHandler
        [ ( Int
          , UTCTime
          , Int
          , Int
          , Bool
          )
        ]
    )
  liftIO $ head <$> runInsert_ conn (Insert
    { iTable = amountTable
    , iRows  =
      [
      ( C.constant pid
      , C.constant now
      , C.constant (amount - pdamount)
      , C.constant oldprice
      , C.constant False
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _) -> id_)
    , iOnConflict = Nothing
    }
    )
