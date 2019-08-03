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
  "user" :>
    ( "list" :> AuthProtect "header-auth"
      :> QueryParam "refine" Refine :> Get '[JSON] [User]
    :<|> "new" :> ReqBody '[JSON] UserSubmit :> Post '[JSON] Int
    :<|> "details" :> AuthProtect "header-auth"
      :> Capture "id" Int :> Get '[JSON] UserDetails
    :<|> "details" :> AuthProtect "header-auth"
      :> Capture "id" Int :> ReqBody '[JSON] UserDetailsSubmit :> Post '[JSON] ()
    )
  :<|> "product" :>
    ( "list" :> Get '[JSON] [ProductShortOverview]
    :<|> "list" :> AuthProtect "header-auth" :> Get '[JSON] [ProductOverview]
    :<|> "new" :> AuthProtect "header-auth" :> ReqBody '[JSON] ProductSubmit
      :> Post '[JSON] Int
    :<|> "update" :> AuthProtect "header-auth"
      :> ReqBody '[JSON] AmountUpdate :> Post '[JSON] ()
    )
  :<|> "buy" :> AuthProtect "header-auth" :> ReqBody '[JSON] [PurchaseDetail]
    :> Post '[JSON] PurchaseResult
  :<|> "auth" :> 
    ( "get" :> ReqBody '[JSON] Int :> Post '[JSON] AuthInfo
    :<|> "send" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResult
    :<|> "logout" :> AuthProtect "header-auth" :> ReqBody '[JSON] Int
      :> Post '[JSON] ()
    )
