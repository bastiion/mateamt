{-# LANGUAGE OverloadedStrings #-}
module Model.Amount where

import Data.Profunctor.Product (p5)

import qualified Database.PostgreSQL.Simple as PGS

import Opaleye as O
import Opaleye.Constant as C

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
  , Field SqlDate
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlBool
  )
  ( Field SqlInt4
  , Field SqlDate
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
