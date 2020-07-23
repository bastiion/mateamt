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

roleNew
  :: Maybe (Int, AuthMethod)
  -> RoleSubmit
  -> MateHandler Int
roleNew (Just (_, auth)) (RoleSubmit name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11) =
  insertRole name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =<< asks rsConnection
roleNew Nothing _ ->
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleUpdate
  :: Maybe (Int, AuthMethod)
  -> Role
  -> MateHandler ()
roleUpdate Just (_, auth) role =
  updateRole role =<< asks rsConnection
roleUpdate Nothing _ ->
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleDelete _ _ = notImplemented

roleAssociationList
  :: MateHandler [RoleAssociation]
roleAssociationList =
  selectAllRoleAssociations =<< asks rsConnection

roleAssociationSubmit _ _ = notImplemented

roleAssociationDelete _ _ = notImplemented

notImplemented :: MateHandler a
notImplemented = throwError $ err501
  { errBody = "Function has not yet been implemented!"
  }
