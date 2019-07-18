{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Product where

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

initProduct :: PGS.Query
initProduct = "create Table if not exists \"product\" (product_id serial primary key, product_ident varchar(128) not null, product_price integer not null, product_amount integer not null, product_vanish integer not null, product_ml integer not null, product_avatar integer, product_supplier integer, product_max_amount integer not null, product_total_bought integer not null, product_amount_per_crate integer not null, product_price_per_crate integer, product_art_nr varchar(128))"

productTable :: Table
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
productTable = table "product" (
  p13
    ( tableField "product_id"
    , tableField "product_ident"
    , tableField "product_price"
    , tableField "product_amount"
    , tableField "product_vanish"
    , tableField "product_ml"
    , tableField "product_avatar"
    , tableField "product_supplier"
    , tableField "product_max_amount"
    , tableField "product_total_bought"
    , tableField "product_amount_per_crate"
    , tableField "product_price_per_crate"
    , tableField "product_art_nr"
    )
  )

productSelect
  :: PGS.Connection
  -> MateHandler [Product]
productSelect conn = do
  bevs <- liftIO $ runSelect conn
    ( keepWhen (\_ -> C.constant True) <<< queryTable productTable
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
      Product i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13
      )
    bevs


insertProduct
  :: ProductSubmit
  -> Insert [Int]
insertProduct (ProductSubmit ident price ml ava sup max apc ppc artnr) = Insert
  { iTable = productTable
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


updateProduct
  :: Int
  -> ProductSubmit
  -> Update Int64
updateProduct sid (ProductSubmit ident price ml ava sup max apc ppc artnr) = Update
  { uTable      = productTable
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
