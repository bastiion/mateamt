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

type UserAPI = "user" :>
  ( "list" :> QueryParam "refine" Refine :> Get '[JSON] [User]
  :<|> "new" :> ReqBody '[JSON] UserSubmit :> Post '[JSON] Int
  )

data SortBy = Name
