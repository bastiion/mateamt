{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text

import GHC.Generics

import Web.HttpApiData

data Refine = All | Old
  deriving (Generic, Show, Enum)

instance FromHttpApiData Refine where
  parseQueryParam t =
    case t of
      "all" -> Right All
      "old" -> Right Old
      x     -> Left ("Error: Unknown refine " <> x)
