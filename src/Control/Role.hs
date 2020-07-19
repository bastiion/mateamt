{-# LANGUAGE OverloadedStrings #-}
module Control.Role where

import Servant

import Control.Monad (void, when)

import Control.Monad.Reader (asks)

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

-- internal imports

import Types
import Model

roleList :: MateHandler [Role]
roleList = do
  conn <- asks rsConnection
  selectAllRoles conn

roleNew = notImpelemented

roleUpdate = notImpelemented

roleDelete = notImpelemented

roleAssociationList = notImpelemented

roleAssociationSubmit = notImpelemented

roleAssociationDelete = notImpelemented

notImplemented = throwError $ err501
  { errBody = "Function has not yet been implemented!"
  }
