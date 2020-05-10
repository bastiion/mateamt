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
