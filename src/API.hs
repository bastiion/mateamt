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
  :<|> "beverage" :>
    ( "list" :> Get '[JSON] [Beverage]
    :<|> "new" :> AuthProtect "header-auth" :> ReqBody '[JSON] BeverageSubmit
      :> Post '[JSON] Int
    )
  :<|> "auth" :> 
    ( "get" :> ReqBody '[JSON] Int :> Post '[JSON] AuthInfo
    :<|> "send" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResult
    )
