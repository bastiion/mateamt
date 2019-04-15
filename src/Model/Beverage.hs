{-# LANGUAGE DeriveGeneric #-}
module Model.Beverage where

import Data.Text as T
import Data.Time.Calendar (Day)
import Data.Profunctor.Product (p12)

import Data.Aeson
import Data.Aeson.Types

import GHC.Generics

import Opaleye as O

data Beverage = Beverage
  { beverageIdent          :: T.Text
  , beveragePrice          :: Int
  , beverageAmount         :: Int
  , beverageVanish         :: Int
  , beverageMl             :: Int
  , beverageAvatar         :: Maybe Word
  , beverageSupplier       :: Maybe Word
  , beverageMaxAmount      :: Int
  , beverageTotalBought    :: Int
  , beverageAmonutPerCrate :: Int
  , beveragePricePerCrate  :: Maybe Int
  , beverageArtNr          :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON Beverage where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Beverage

beverageTable :: Table
  ( Field SqlText
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , FieldNullable SqlInt8
  , FieldNullable SqlText
  )
  ( Field SqlText
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt8
  , Field SqlInt8
  , Field SqlInt8
  , FieldNullable SqlInt8
  , FieldNullable SqlText
  )
beverageTable = table "beverage" (
  p12
    ( tableField "ident"
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
