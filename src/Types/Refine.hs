{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Refine where

import GHC.Generics

import Web.HttpApiData

data UserRefine = AllUsers | ActiveUsers | OldUsers
  deriving (Generic, Show, Enum)

instance FromHttpApiData UserRefine where
  parseQueryParam t =
    case t of
      "all"    -> Right AllUsers
      "active" -> Right ActiveUsers
      "old"    -> Right OldUsers
      x        -> Left ("Error: Unknown refine " <> x)

instance ToHttpApiData UserRefine where
  toUrlPiece x = case x of
    AllUsers    -> "all"
    ActiveUsers -> "active"
    OldUsers    -> "old"


data ProductRefine = AllProducts | AvailableProducts | DepletedProducts
  deriving (Generic, Show, Enum)

instance FromHttpApiData ProductRefine where
  parseQueryParam t =
    case t of
      "all"       -> Right AllProducts
      "available" -> Right AvailableProducts
      "depleted"  -> Right DepletedProducts
      x     -> Left ("Error: Unknown refine " <> x)

instance ToHttpApiData ProductRefine where
  toUrlPiece x = case x of
    AllProducts       -> "all"
    AvailableProducts -> "available"
    DepletedProducts  -> "depleted"
