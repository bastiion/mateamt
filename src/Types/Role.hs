module Types.Role where

import qualified Data.Text as T

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

data RoleAssociation = RoleAssociation
  { roleAssociationId   :: Int
  , roleAssociationUser :: Int
  , roleAssociationRole :: Int
  }

data RoleAssociationSubmit = RoleAssociationSubmit
  { roleAssociationSubmitUser :: Int
  , roleAssociationSubmitRole :: Int
  }
