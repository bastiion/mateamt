{-# LANGUAGE OverloadedStrings #-}
module Control.Meta where

import Control.Monad.Reader (ask)

-- internal imports

import Types

metaGet :: MateHandler MetaInformation
metaGet = do
  (ReadState _ _ symbol version) <- ask
  return (MetaInformation
    { metaInfoVersion = version
    , metaInfoCurrency = symbol
    }
    )
