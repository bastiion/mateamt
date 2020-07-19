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

roleNew _ _ = notImplemented

roleUpdate _ _ = notImplemented

roleDelete _ _ = notImplemented

roleAssociationList = notImplemented

roleAssociationSubmit _ _ = notImplemented

roleAssociationDelete _ _ = notImplemented

notImplemented :: MateHandler a
notImplemented = throwError $ err501
  { errBody = "Function has not yet been implemented!"
  }
