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

import Servant.API
import Servant.RawM

-- internal imports

import Types

type MateAPI =
  "auth" :> "get" :> ReqBody '[JSON] TicketRequest :> Post '[JSON] AuthInfo
  :<|> "auth" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResult
  :<|> "auth" :> AuthProtect "header-auth" :> ReqBody '[JSON] Int
    :> Delete '[JSON] ()

  :<|> "user" :> ReqBody '[JSON] UserSubmit :> Post '[JSON] Int
  :<|> "user" :> AuthProtect "header-auth"
    :> Capture "uid'" Int :> Get '[JSON] UserDetails
  :<|> "user" :> AuthProtect "header-auth"
    :> Capture "uid'" Int :> ReqBody '[JSON] UserDetailsSubmit :> Patch '[JSON] ()
  :<|> "user" :> "list" :> QueryParam "refine" UserRefine :> Get '[JSON] [UserSummary]
  :<|> "user" :> "recharge" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] UserRecharge :> Post '[JSON] ()
  :<|> "user" :> "transfer" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] UserTransfer :> Post '[JSON] ()

  :<|> "product" :> AuthProtect "header-auth" :> ReqBody '[JSON] ProductSubmit
    :> Post '[JSON] Int
  :<|> "product" :> Capture "pid" Int :> Get '[JSON] ProductOverview
  :<|> "product" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] [AmountRefill] :> Patch '[JSON] ()
  :<|> "product" :> AuthProtect "header-auth"
    :> ReqBody '[JSON] [AmountUpdate] :> Put '[JSON] ()
  :<|> "product" :> "list" :> QueryParam "refine" ProductRefine
    :> Get '[JSON] [ProductOverview]

  :<|> "buy" :> AuthProtect "header-auth" :> ReqBody '[JSON] [PurchaseDetail]
    :> Post '[JSON] PurchaseResult

  :<|> "journal" :> AuthProtect "header-auth" :> QueryParam "limit" Int
    :> QueryParam "offset" Int :> Get '[JSON] [JournalEntry]

  :<|> "avatar" :> Capture "id" Int :> RawM
  :<|> "avatar" :> AuthProtect "header-auth" :> ReqBody '[JSON] AvatarData
    :> Post '[JSON] Int
  :<|> "avatar" :> AuthProtect "header-auth" :> Capture "id" Int
    :> ReqBody '[JSON] AvatarData :> Patch '[JSON] ()
  :<|> "avatar" :> "list" :> Get '[JSON] [Avatar]
