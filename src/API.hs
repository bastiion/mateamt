{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API where

import Data.Text
import Data.Time (UTCTime)

import Servant.API

-- internal imports

import Model as M

import Types

type UserAPI =
  "auth" :> Capture "uid" Int :> Get '[JSON] AuthInfo
  :<|> "auth" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResult
  :<|> "auth" :> AuthProtect "header-auth" :> ReqBody '[JSON] Int
    :> Delete '[JSON] ()

  :<|> "user" :> ReqBody '[JSON] UserSubmit :> Post '[JSON] Int
  :<|> "user" :> AuthProtect "header-auth"
    :> Capture "uid'" Int :> Get '[JSON] UserDetails
  :<|> "user" :> AuthProtect "header-auth"
    :> Capture "uid'" Int :> ReqBody '[JSON] UserDetailsSubmit :> Patch '[JSON] ()
  :<|> "user" :> "list" :> AuthProtect "header-auth"
    :> QueryParam "refine" Refine :> Get '[JSON] [User]

  :<|> "product" :> AuthProtect "header-auth" :> ReqBody '[JSON] ProductSubmit
    :> Post '[JSON] Int
  :<|> "product" :> Capture "pid" Int :> Get '[JSON] ProductOverview
  :<|> "product" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] [AmountRefill] :> Patch '[JSON] ()
  :<|> "product" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] [AmountUpdate] :> Put '[JSON] ()
  :<|> "product" :> "list" :> Get '[JSON] [ProductOverview]

  :<|> "buy" :> AuthProtect "header-auth" :> ReqBody '[JSON] [PurchaseDetail]
    :> Post '[JSON] PurchaseResult

  :<|> "journal" :> AuthProtect "header-auth" :> QueryParam "limit" Int
    :> QueryParam "offset" Int :> Get '[JSON] [JournalEntry]
