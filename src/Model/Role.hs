{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model.Role where

import qualified Database.PostgreSQL.Simple as PGS

import Data.Profunctor.Product (p2, p12)

import qualified Data.Text as T

import Data.Int (Int64)

import Opaleye as O hiding (null, not)
import Opaleye.Constant as C

import Control.Arrow ((<<<))

import Control.Monad
import Control.Monad.IO.Class (liftIO)

-- internal imports

import Types
import Classes

initRole :: PGS.Query
initRole = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"role\" ("
  , "role_id                    SERIAL NOT NULL,"
  , "role_name                  TEXT NOT NULL,"
  , "role_can_refill_stock      BOOLEAN NOT NULL,"
  , "role_can_pay_invoice       BOOLEAN NOT NULL,"
  , "role_can_pay_out           BOOLEAN NOT NULL,"
  , "role_can_manage_products   BOOLEAN NOT NULL,"
  , "role_can_manage_journal    BOOLEAN NOT NULL,"
  , "role_can_manage_roles      BOOLEAN NOT NULL,"
  , "role_can_manage_suppliers  BOOLEAN NOT NULL,"
  , "role_can_manage_avatars    BOOLEAN NOT NULL,"
  , "role_can_manage_settings   BOOLEAN NOT NULL,"
  , "PRIMARY KEY (role_id),"
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
  )
roleTable = table "role" (
  p12
    ( tableField "role_id"
    , tableField "role_name"
    , tableField "role_can_refill_stock"
    , tableField "role_can_view_journal"
    , tableField "role_can_pay_invoice"
    , tableField "role_can_pay_out"
    , tableField "role_can_manage_products"
    , tableField "role_can_manage_journal"
    , tableField "role_can_manage_roles"
    , tableField "role_can_manage_suppliers"
    , tableField "role_can_manage_avatars"
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
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _, _, _, _) -> id_ )
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
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _, _, _, _) -> id_ )
    , iOnConflict = Nothing
    }
  return $ a ++ b

selectAllRoles
  :: PGS.Connection
  -> MateHandler [Role]
selectAllRoles conn = do
  liftIO $ map fromDatabase <$> runSelect conn (
    queryTable roleTable
    ) :: MateHandler [Role]

selectRoleList
  :: [Int]
  -> PGS.Connection
  -> MateHandler [Role]
selectRoleList ids conn = do
  liftIO $ map fromDatabase <$> runSelect conn (
    keepWhen (\(id_, _, _, _, _, _, _, _, _, _, _, _) ->
      in_ (map C.constant ids) id_)
      <<< queryTable roleTable
    ) :: MateHandler [Role]

insertRole
  :: T.Text
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> PGS.Connection
  -> MateHandler Int
insertRole name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 conn = do
  head <$> liftIO (runInsert_ conn $ Insert
    { iTable = roleTable
    , iRows =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant name
      , C.constant c1
      , C.constant c2
      , C.constant c3
      , C.constant c4
      , C.constant c5
      , C.constant c6
      , C.constant c7
      , C.constant c8
      , C.constant c9
      , C.constant c10
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _, _, _, _) -> id_ )
    , iOnConflict = Nothing
    })


queryRoleIdByName
  :: T.Text
  -> PGS.Connection
  -> MateHandler Int
queryRoleIdByName name conn = do
  liftIO $ roleID . fromDatabase . head <$> runSelect conn (
    keepWhen (\(_, rname, _, _, _, _, _, _, _, _, _, _) ->
      C.constant name .== rname) <<< queryTable roleTable
    ) :: MateHandler Int

queryRoleIdByCapabilities
  :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
  -> PGS.Connection
  -> MateHandler Int
queryRoleIdByCapabilities (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) conn =
  do
    liftIO $ roleID . fromDatabase . head <$> runSelect conn (
      keepWhen (\(_, _, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) ->
        C.constant p1 .== c1 .&&
        C.constant p2 .== c2 .&&
        C.constant p3 .== c3 .&&
        C.constant p4 .== c4 .&&
        C.constant p5 .== c5 .&&
        C.constant p6 .== c6 .&&
        C.constant p7 .== c7 .&&
        C.constant p8 .== c8 .&&
        C.constant p9 .== c9 .&&
        C.constant p10 .== c10
        )
          <<< queryTable roleTable
      ) :: MateHandler Int


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
    (uncurry RoleAssociation)
    rawRoleAssocs


selectUserAssociations
  :: Int
  -> PGS.Connection
  -> MateHandler [RoleAssociation]
selectUserAssociations uid conn = do
  rawAssocs <- liftIO $ runSelect conn(
    keepWhen (\(auid, _) -> auid .== C.constant uid)
      <<< queryTable userToRoleTable
    ) :: MateHandler
      [
        ( Int
        , Int
        )
      ]
  return $ map (uncurry RoleAssociation) rawAssocs


associateUserToRole
  :: Int             -- ^ User id
  -> Int             -- ^ Role id
  -> PGS.Connection
  -> MateHandler ()
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


deleteAssociation
  :: Int             -- ^ User id
  -> Int             -- ^ Role id
  -> PGS.Connection
  -> MateHandler Int64
deleteAssociation uid rid conn =
  liftIO $ runDelete_ conn $ Delete
    { dTable     = userToRoleTable
    , dWhere     =
      \(auid, arid) -> auid .== C.constant uid .&& arid .== C.constant rid
    , dReturning = rCount
    }


updateRole
  :: Int        -- ID of the updated role
  -> RoleSubmit -- The role with already updated info
  -> PGS.Connection
  -> MateHandler Int64
updateRole rid role@(RoleSubmit name c1 c2 c3 c4 c5 c6 c7 c8 c9 c10) conn =
  liftIO $ runUpdate_ conn $ Update
    { uTable      = roleTable
    , uUpdateWith = updateEasy (\(id_, _, _, _, _, _, _, _, _, _, _, _) ->
        ( id_
        , C.constant name
        , C.constant c1
        , C.constant c2
        , C.constant c3
        , C.constant c4
        , C.constant c5
        , C.constant c6
        , C.constant c7
        , C.constant c8
        , C.constant c9
        , C.constant c10
        )
      )
    , uWhere      = \(id_, _, _, _, _, _, _, _, _, _, _, _) ->
      id_ .== C.constant rid
    , uReturning = rCount
    }

deleteRole
  :: Int
  -> PGS.Connection
  -> MateHandler Int64
deleteRole rid conn =
  liftIO $ runDelete_ conn $ Delete
    { dTable     = roleTable
    , dWhere     =
      \(id_, _, _, _, _, _, _, _, _, _, _, _) -> id_ .== C.constant rid
    , dReturning = rCount
    }
