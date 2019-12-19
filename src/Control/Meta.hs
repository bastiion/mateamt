{-# LANGUAGE OverloadedStrings #-}
module Control.Meta where

import Servant

import Control.Monad.Reader (ask)

-- internal imports

import Types
import Model

metaGet :: MateHandler MetaInformation
metaGet = do
  (ReadState _ _ symbol version) <- ask
  return (MetaInformation
    { metaInfoVersion = version
    , metaInfoCurrency = symbol
    }
    )
