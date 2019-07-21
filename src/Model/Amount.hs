{-# LANGUAGE OverloadedStrings #-}
module Model.Amount where

import Data.Profunctor.Product (p5)

import qualified Database.PostgreSQL.Simple as PGS

import Opaleye as O
import Opaleye.Constant as C

initAmount :: PGS.Query
initAmount = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"amount\" ("
  , "amounts_product_id BIGINT      NOT NULL REFERENCES product ON DELETE CASCADE,"
  , "amounts_timestamp  TIMESTAMPTZ NOT NULL,"
  , "amounts_amount     INTEGER     NOT NULL,"
  , "amounts_price      INTEGER     NOT NULL,"
  , "amounts_verified   BOOLEAN     NOT NULL,"
  , "PRIMARY KEY (product_id, timestamp)"
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
    ( tableField "amonnts_product_id"
    , tableField "amounts_timestamp"
    , tableField "amounts_amount"
    , tableField "amounts_price"
    , tableField "amounts_verified"
    )
  )
