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
initBeverage = "create Table if not exists \"beverage\" (id serial primary key, ident varchar(128) not null, price integer not null, amount integer not null, vanish integer not null, ml integer not null, avatar integer, supplier integer, max_amount integer not null, total_bought integer not null, amount_per_crate integer not null, price_per_crate integer, art_nr varchar(128))"

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
    ( tableField "id"
    , tableField "ident"
    , tableField "price"
    , tableField "amount"
    , tableField "vanish"
    , tableField "ml"
    , tableField "avatar"
    , tableField "supplier"
    , tableField "max_amount"
    , tableField "total_bought"
    , tableField "amount_per_crate"
    , tableField "price_per_crate"
    , tableField "art_nr"
    )
  )
