{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Beverage where

import Data.Text as T
import Data.Time.Calendar (Day)
import Data.Profunctor.Product (p13)

import Data.Aeson
import Data.Aeson.Types

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

import Opaleye as O

initBeverage :: PGS.Query
initBeverage = "create Table if not exists \"beverage\" (beverage_id serial primary key, beverage_ident varchar(128) not null, beverage_price integer not null, beverage_amount integer not null, beverage_vanish integer not null, beverage_ml integer not null, beverage_avatar integer, beverage_supplier integer, beverage_max_amount integer not null, beverage_total_bought integer not null, beverage_amount_per_crate integer not null, beverage_price_per_crate integer, beverage_art_nr varchar(128))"

beverageTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
beverageTable = table "beverage" (
  p13
    ( tableField "beverage_id"
    , tableField "beverage_ident"
    , tableField "beverage_price"
    , tableField "beverage_amount"
    , tableField "beverage_vanish"
    , tableField "beverage_ml"
    , tableField "beverage_avatar"
    , tableField "beverage_supplier"
    , tableField "beverage_max_amount"
    , tableField "beverage_total_bought"
    , tableField "beverage_amount_per_crate"
    , tableField "beverage_price_per_crate"
    , tableField "beverage_art_nr"
    )
  )
