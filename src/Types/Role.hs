{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Types.Role where

import qualified Data.Text as T
import Data.Aeson

import GHC.Generics

-- internal imports

import Classes.ToDatabase
import Classes.FromDatabase

data Role = Role
  { roleID                 :: Int
  , roleName               :: T.Text
  , roleCanRefillStock     :: Bool
  -- | paying invoice only adds to user funds
  , roleCanPayInvoice      :: Bool
  -- | paying out actually removes money from the cashier
  , roleCanPayOut          :: Bool
  , roleCanManageProducts  :: Bool
  , roleCanManageJournal   :: Bool
  , roleCanManageUsers     :: Bool
  , roleCanManageRoles     :: Bool
  , roleCanManageSuppliers :: Bool
  , roleCanManageAvatars   :: Bool
  , roleCanManageSettings  :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Role

instance ToDatabase Role where

  type InTuple Role =
    (Int, T.Text, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

  toDatabase (Role id_ name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10) =
    (id_, name, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)

instance FromDatabase Role where

  type OutTuple Role =
    (Int, T.Text, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

  fromDatabase (id_, name, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) =
    Role id_ name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

data RoleSubmit = RoleSubmit
  { roleSubmitName               :: T.Text
  , roleSubmitCanRefillStock     :: Bool
  , roleSubmitCanPayInvoice      :: Bool
  , roleSubmitCanPayOut          :: Bool
  , roleSubmitCanManageProducts  :: Bool
  , roleSubmitCanManageJournal   :: Bool
  , roleSubmitCanManageUsers     :: Bool
  , roleSubmitCanManageRoles     :: Bool
  , roleSubmitCanManageSuppliers :: Bool
  , roleSubmitCanManageAvatars   :: Bool
  , roleSubmitCanManageSettings  :: Bool
  }
  deriving (Generic, Show)

instance ToJSON RoleSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoleSubmit

data RoleAssociation = RoleAssociation
  { roleAssociationUser :: Int
  , roleAssociationRole :: Int
  }
  deriving (Generic, Show)

instance ToJSON RoleAssociation where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoleAssociation

data RoleAssociationSubmit = RoleAssociationSubmit
  { roleAssociationSubmitUser :: Int
  , roleAssociationSubmitRole :: Int
  }
  deriving (Generic, Show)

instance ToJSON RoleAssociationSubmit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoleAssociationSubmit
