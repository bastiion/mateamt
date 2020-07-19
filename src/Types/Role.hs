{-# LANGUAGE DeriveGeneric #-}
module Types.Role where

import qualified Data.Text as T
import Data.Aeson

import GHC.Generics

data Role = Role
  { roleID                 :: Int
  , roleName               :: T.Text
  , roleCanRefillStock     :: Bool
  , roleCanAddProduct      :: Bool
  , roleCanViewJournal     :: Bool
  , roleCanPayInvoice      :: Bool
  , roleCanPayOut          :: Bool
  , roleCanManageProducts  :: Bool
  , roleCanManageJournal   :: Bool
  , roleCanManageUsers     :: Bool
  , roleCanManageRoles     :: Bool
  , roleCanManageSuppliers :: Bool
  , roleCanManageSettings  :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Role

data RoleSubmit = RoleSubmit
  { roleSubmitName               :: T.Text
  , roleSubmitCanRefillStock     :: Bool
  , roleSubmitCanAddProduct      :: Bool
  , roleSubmitCanViewJournal     :: Bool
  , roleSubmitCanPayInvoice      :: Bool
  , roleSubmitCanPayOut          :: Bool
  , roleSubmitCanManageProducts  :: Bool
  , roleSubmitCanManageJournal   :: Bool
  , roleSubmitCanManageUsers     :: Bool
  , roleSubmitCanManageRoles     :: Bool
  , roleSubmitCanManageSuppliers :: Bool
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
