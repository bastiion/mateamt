{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model.Role where

import qualified Database.PostgreSQL.Simple as PGS

import Data.Profunctor.Product (p2, p13)

import qualified Data.Text as T

import Opaleye as O hiding (null, not)
import Opaleye.Constant as C

import Control.Arrow ((<<<))

import Control.Monad
import Control.Monad.IO.Class (liftIO)

-- internal imports

import Types

initRole :: PGS.Query
initRole = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"role\" ("
  , "role_id                    SERIAL NOT NULL,"
  , "role_name                  TEXT NOT NULL,"
  , "role_can_refill_stock      BOOLEAN NOT NULL,"
  , "role_can_add_product       BOOLEAN NOT NULL,"
  , "role_can_view_journal      BOOLEAN NOT NULL,"
  , "role_can_pay_invoice       BOOLEAN NOT NULL,"
  , "role_can_pay_out           BOOLEAN NOT NULL,"
  , "role_can_manage_products   BOOLEAN NOT NULL,"
  , "role_can_manage_journal    BOOLEAN NOT NULL,"
  , "role_can_manage_users      BOOLEAN NOT NULL,"
  , "role_can_manage_roles      BOOLEAN NOT NULL,"
  , "role_can_manage_suppliers  BOOLEAN NOT NULL,"
  , "role_can_manage_settings   BOOLEAN NOT NULL,"
  , "PRIMARY KEY (role_id)"
  , "UNIQUE (role_name)"
  , ")"
  ]

initUserToRole :: PGS.Query
initUserToRole = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"user_to_role\" ("
  , "user_id    INTEGER NOT NULL REFERENCES \"user\"(\"user_id\"),"
  , "role_id    INTEGER NOT NULL REFERENCES \"role\"(\"role_id\"),"
  , "PRIMARY KEY (user_id, role_id)"
  , ")"
  ]

roleTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  , Field SqlBool
  )
roleTable = table "role" (
  p13
    ( tableField "role_id"
    , tableField "role_name"
    , tableField "role_can_refill_stock"
    , tableField "role_can_add_product"
    , tableField "role_can_view_journal"
    , tableField "role_can_pay_invoice"
    , tableField "role_can_pay_out"
    , tableField "role_can_manage_products"
    , tableField "role_can_manage_journal"
    , tableField "role_can_manage_users"
    , tableField "role_can_manage_roles"
    , tableField "role_can_manage_suppliers"
    , tableField "role_can_manage_settings"
    )
  )

userToRoleTable :: Table
  ( Field SqlInt4
  , Field SqlInt4
  )
  ( Field SqlInt4
  , Field SqlInt4
  )
userToRoleTable = table "user_to_role" (
  p2
    ( tableField "user_id"
    , tableField "role_id"
    )
  )

insertInitialRoles :: PGS.Connection -> MateHandler ()
insertInitialRoles conn =
  void $ liftIO $ runInsertInitialRoles conn

runInsertInitialRoles :: PGS.Connection -> IO [Int]
runInsertInitialRoles conn = do
  a <- runInsert_ conn $ Insert
    { iTable = roleTable
    , iRows =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant ("Administrator" :: String)
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      , C.constant True
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _, _, _, _, _) -> id_ )
    , iOnConflict = Nothing
    }
  b <- runInsert_ conn $ Insert
    { iTable = roleTable
    , iRows =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant ("User" :: String)
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      , C.constant False
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _, _, _, _, _) -> id_ )
    , iOnConflict = Nothing
    }
  return $ a ++ b

selectAllRoles
  :: PGS.Connection
  -> MateHandler [Role]
selectAllRoles conn = do
  rawRoles <- liftIO $ runSelect conn (
    queryTable roleTable
    ) :: MateHandler
        [
          ( Int
          , T.Text
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          )
        ]
  return $ map
    (\(id_, name, c1, c2, c3, c4, c5, c6, c7, c8, c9 ,c10, c11) ->
      Role id_ name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11)
    rawRoles


queryRoleIdByName
  :: T.Text
  -> PGS.Connection
  -> MateHandler Int
queryRoleIdByName name conn = do
  roles <- liftIO $ runSelect conn (
    keepWhen (\(_, rname, _, _, _, _, _, _, _, _, _, _, _) ->
      C.constant name .== rname) <<< queryTable roleTable
    ) :: MateHandler
        [
          ( Int
          , T.Text
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          , Bool
          )
        ]
  return $ (\(rid, _, _, _, _, _, _, _, _, _, _, _, _) -> rid) (head roles)

queryRoleIdByCapabilities
  :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
  -> PGS.Connection
  -> MateHandler Int
queryRoleIdByCapabilities (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) conn =
  do
    roles <- liftIO $ runSelect conn (
      keepWhen (\(_, _, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) ->
        C.constant p1 .== c1 .&&
        C.constant p2 .== c2 .&&
        C.constant p3 .== c3 .&&
        C.constant p4 .== c4 .&&
        C.constant p5 .== c5 .&&
        C.constant p6 .== c6 .&&
        C.constant p7 .== c7 .&&
        C.constant p8 .== c8 .&&
        C.constant p9 .== c9 .&&
        C.constant p10 .== c10 .&&
        C.constant p11 .== c11
        )
          <<< queryTable roleTable
      ) :: MateHandler
          [
            ( Int
            , T.Text
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            , Bool
            )
          ]
    return $ (\(rid, _, _, _, _, _, _, _, _, _, _, _, _) -> rid) (head roles)


selectAllRoleAssociations
  :: PGS.Connection
  -> MateHandler [RoleAssociation]
selectAllRoleAssociations conn = do
  rawRoleAssocs <- liftIO $ runSelect conn (
    queryTable userToRoleTable
    ) :: MateHandler
        [
          ( Int
          , Int
          )
        ]
  return $ map
    (\(uid, rid) -> RoleAssociation uid rid)
    rawRoleAssocs


associateUserToRole
  :: Int             -- ^ User id
  -> Int             -- ^ Role id
  -> PGS.Connection
  -> MateHandler () -- ^ Resulting UserToRole id
associateUserToRole uid rid conn =
  head <$> liftIO (runInsert_ conn $ Insert
    { iTable = userToRoleTable
    , iRows =
      [
      ( C.constant uid
      , C.constant rid
      )
      ]
    , iReturning = rReturning (const ())
    , iOnConflict = Nothing
    })
