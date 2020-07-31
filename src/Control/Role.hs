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
roleNew (Just (uid, auth)) (RoleSubmit name c1 c2 c3 c4 c5 c6 c7 c8 c9) =
  do
    isRoleManager <- checkCapability uid roleCanManageRoles
    if (auth `elem` [PrimaryPass, ChallengeResponse] && isRoleManager)
    then
      insertRole name c1 c2 c3 c4 c5 c6 c7 c8 c9 =<< asks rsConnection
    else
      throwError $ err401
        { errBody = "You are not authorized for this action."
        }
roleNew Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleUpdate
  :: Maybe (Int, AuthMethod)
  -> Int
  -> RoleSubmit
  -> MateHandler ()
roleUpdate (Just (uid, auth)) id_ roleSubmit = do
  isRoleManager <- checkCapability uid roleCanManageRoles
  if (auth `elem` [PrimaryPass, ChallengeResponse] && isRoleManager)
  then
    void $ updateRole id_ roleSubmit =<< asks rsConnection
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
roleUpdate Nothing _ _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleDelete
  :: Maybe (Int, AuthMethod)
  -> Int
  -> MateHandler ()
roleDelete (Just (uid, auth)) id_ = do
  isRoleManager <- checkCapability uid roleCanManageRoles
  if (auth `elem` [PrimaryPass, ChallengeResponse] && isRoleManager)
  then
    void $ deleteRole id_ =<< asks rsConnection
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
roleDelete Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleAssociationList
  :: MateHandler [RoleAssociation]
roleAssociationList =
  selectAllRoleAssociations =<< asks rsConnection

roleAssociationSubmit
  :: Maybe (Int, AuthMethod)
  -> RoleAssociationSubmit
  -> MateHandler ()
roleAssociationSubmit (Just (uid, auth)) (RoleAssociationSubmit auid arid) = do
  isRoleManager <- checkCapability uid roleCanManageRoles
  if (auth `elem` [PrimaryPass, ChallengeResponse] && isRoleManager)
  then
    associateUserToRole auid arid =<< asks rsConnection
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
roleAssociationSubmit Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

roleAssociationDelete
  :: Maybe (Int, AuthMethod)
  -> RoleAssociation
  -> MateHandler ()
roleAssociationDelete (Just (uid, auth)) (RoleAssociation auid arid) = do
  isRoleManager <- checkCapability uid roleCanManageRoles
  if (auth `elem` [PrimaryPass, ChallengeResponse] && isRoleManager)
  then
    void $ deleteAssociation auid arid =<< asks rsConnection
  else
    throwError $ err401
      { errBody = "You are not authorized for this action."
      }
roleAssociationDelete Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

-- | This is the internal function to check a users authorization to do certain
-- actions
checkCapability
  :: Int              -- ^ User Id to check
  -> (Role -> Bool)   -- ^ Predicate to check
  -> MateHandler Bool -- ^ Result
checkCapability uid accessRule = do
  conn <- asks rsConnection
  assocs <- selectUserAssociations uid conn
  let rids = map roleAssociationRole assocs
  roles <- selectRoleList rids conn
  return $ any accessRule roles

notImplemented :: MateHandler a
notImplemented = throwError $ err501
  { errBody = "Function has not yet been implemented!"
  }
