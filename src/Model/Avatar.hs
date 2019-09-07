{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Model.Avatar where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS (fromChunks)

import Data.Int (Int64)

import Data.Profunctor.Product (p4)

import qualified Data.Digest.Pure.MD5 as MD5

import qualified Database.PostgreSQL.Simple as PGS

import Control.Arrow

import Control.Monad.IO.Class (liftIO)

import Opaleye as O
import Opaleye.Constant as C

-- internal imports

import Types

initAvatar :: PGS.Query
initAvatar = mconcat
  [ "CREATE TABLE IF NOT EXISTS \"avatar\" ("
  , "avatar_id   SERIAL PRIMARY KEY,"
  , "avatar_name TEXT   NOT NULL,"
  , "avatar_hash BYTEA  NOT NULL,"
  , "avatar_data BYTEA  NOT NULL"
  , ")"
  ]

avatarTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlBytea
  , Field SqlBytea
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlBytea
  , Field SqlBytea
  )
avatarTable = table "avatar" (
  p4
    ( tableField "avatar_id"
    , tableField "avatar_name"
    , tableField "avatar_hash"
    , tableField "avatar_data"
    )
  )

avatarSelect
  :: PGS.Connection
  -> IO [Avatar]
avatarSelect conn = liftIO $ do
  avatars <- runSelect conn (
    proc () -> do
      ret <- orderBy (desc (\(_, name, _, _) -> name))
        (queryTable avatarTable) -< ()
      returnA -< ret
    )
    :: IO
      [ ( Int
        , T.Text
        , BS.ByteString
        , BS.ByteString
        )
      ]
  mapM
    (\(daid, dname, dhash, ddata) -> return $
      Avatar daid dname (decodeUtf8 dhash) (decodeUtf8 ddata)
      )
    avatars

avatarSelectById
  :: Int
  -> PGS.Connection
  -> IO [Avatar]
avatarSelectById aid conn = do
  avatars <- runSelect conn (
    keepWhen (\(aaid, _, _, _) -> aaid .== C.constant aid)
      <<< queryTable avatarTable)
    :: IO
      [ ( Int
        , T.Text
        , BS.ByteString
        , BS.ByteString
        )
      ]
  mapM
    (\(daid, dname, dhash, ddata) -> return $
      Avatar daid dname (decodeUtf8 dhash) (decodeUtf8 ddata)
      )
    avatars

insertAvatar
  :: AvatarData
  -> PGS.Connection
  -> MateHandler Int
insertAvatar (AvatarData name dat) conn = fmap head $ liftIO $ do
    let hash = MD5.md5DigestBytes $ MD5.md5 $ BS.fromChunks [encodeUtf8 dat]
    runInsert_ conn $ Insert
      { iTable = avatarTable
      , iRows  =
        [
        ( C.constant (Nothing :: Maybe Int)
        , C.constant name
        , C.constant hash
        , C.constant (encodeUtf8 dat)
        )
        ]
      , iReturning = rReturning (\(aid, _, _, _) -> aid)
      , iOnConflict = Nothing
      }

updateAvatar
  :: Int
  -> AvatarData
  -> PGS.Connection
  -> MateHandler Int64
updateAvatar aid (AvatarData name dat) conn = liftIO $ do
  let hash = MD5.md5DigestBytes $ MD5.md5 $ BS.fromChunks [encodeUtf8 dat]
  runUpdate_ conn $ Update
    { uTable      = avatarTable
    , uUpdateWith = updateEasy (\(did, _, _, _) ->
        ( did
        , C.constant name
        , C.constant hash
        , C.constant (encodeUtf8 dat)
        )
      )
    , uWhere      = (\(did, _, _, _) -> did .== C.constant aid)
    , uReturning  = rCount
    }
