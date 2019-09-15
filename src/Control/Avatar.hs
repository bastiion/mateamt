{-# LANGUAGE OverloadedStrings #-}
module Control.Avatar where

import Control.Monad (void)

import Control.Monad.Reader (ask)

import Control.Monad.Trans (liftIO)

import Data.Text.Encoding (encodeUtf8)

import Data.ByteString.Builder (byteString)

import Servant

import Network.Wai
import Network.HTTP.Types.Status (status200, status404)
import Network.HTTP.Types.Header (hETag)

-- internal imports

import Types
import Model

avatarGet
  :: Int
  -> MateHandler Application
avatarGet aid = do
  conn <- rsConnection <$> ask
  as <- liftIO $ avatarSelectById aid conn
  if null as
  then
    return ((\_ respond -> respond $ responseStream status404 [] $
      \write flush -> do
        write "No avatar found."
        flush
    ) :: Application)
  else
    return ((\_ respond -> respond $ responseStream status200 [(hETag, encodeUtf8 (avatarHash (head as)))] $
      \write flush -> do
        write $ byteString $ encodeUtf8 $ avatarData $ head as
        flush
    ) :: Application)

avatarInsert
  :: Maybe (Int, AuthMethod)
  -> AvatarData
  -> MateHandler Int
avatarInsert (Just _) ad = do
  conn <- rsConnection <$> ask
  insertAvatar ad conn
avatarInsert Nothing _ =
  throwError $ err401
    { errBody = "No Authentication present."
    }

avatarUpdate
  :: Maybe (Int, AuthMethod)
  -> Int
  -> AvatarData
  -> MateHandler ()
avatarUpdate (Just _) aid ad = do
  conn <- rsConnection <$> ask
  void $ updateAvatar aid ad conn
avatarUpdate Nothing _ _ = do
  throwError $ err401
    { errBody = "No Authentication present."
    }

avatarList
  :: MateHandler [Avatar]
avatarList = do
  conn <- rsConnection <$> ask
  liftIO $ avatarSelect conn
