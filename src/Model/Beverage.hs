{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Beverage where

import Data.Text as T
import Data.Time.Calendar (Day)
import Data.Profunctor.Product (p13)

import Data.Aeson
import Data.Aeson.Types

import Data.Int (Int64)

import Control.Monad.IO.Class (liftIO)

import Control.Arrow ((<<<))

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

import Opaleye as O
import Opaleye.Constant as C

-- internal imports

import Types

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

beverageSelect
  :: PGS.Connection
  -> MateHandler [Beverage]
beverageSelect conn = do
  bevs <- liftIO $ runSelect conn
    ( keepWhen (\_ -> C.constant True) <<< queryTable beverageTable
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          , Int
          , Int
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13) -> return $
      Beverage i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13
      )
    bevs


insertBeverage
  :: BeverageSubmit
  -> Insert [Int]
insertBeverage (BeverageSubmit ident price ml ava sup max apc ppc artnr) = Insert
  { iTable = beverageTable
  , iRows  =
    [
    ( C.constant (Nothing :: Maybe Int)
    , C.constant ident
    , C.constant price
    , C.constant (0 :: Int)
    , C.constant (0 :: Int)
    , C.constant ml
    , C.constant ava
    , C.constant sup
    , C.constant max
    , C.constant (0 :: Int)
    , C.constant apc
    , C.constant ppc
    , C.constant artnr
    )
    ]
  , iReturning = rReturning (\(id, _, _, _, _, _, _, _, _, _, _, _, _) -> id)
  , iOnConflict = Nothing
  }


updateBeverage
  :: Int
  -> BeverageSubmit
  -> Update Int64
updateBeverage sid (BeverageSubmit ident price ml ava sup max apc ppc artnr) = Update
  { uTable      = beverageTable
  , uUpdateWith = updateEasy (\(id_, _, _, amo, van, _, _, _, _, tot, _, _, _) ->
      ( id_
      , C.constant ident
      , C.constant price
      , amo
      , van
      , C.constant ml
      , C.constant ava
      , C.constant sup
      , C.constant max
      , tot
      , C.constant apc
      , C.constant ppc
      , C.constant artnr
      )
    )
  , uWhere      =
    (\(id_, _, _, _, _, _, _, _, _, _, _, _, _) ->
      id_ .== C.constant sid
    )
  , uReturning = rCount
  }
