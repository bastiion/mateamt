{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Model.Product where

import Servant.Server

import Data.Text as T hiding (head, foldl)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Profunctor.Product (p9)

import Data.Aeson
import Data.Aeson.Types

import Data.Int (Int64)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)

import Control.Arrow ((<<<), returnA)

import qualified Database.PostgreSQL.Simple as PGS

import GHC.Generics

import Opaleye as O hiding (max)
import Opaleye.Constant as C

-- internal imports

import Types
import Model.Amount

initProduct :: PGS.Query
initProduct = mconcat
  [ "create table if not exists \"product\" ("
  , "product_id serial primary key,"
  , "product_ident varchar(128) not null,"
  -- , "product_price integer not null,"
  -- , "product_amount integer not null,"
  -- , "product_vanish integer not null,"
  , "product_ml integer not null,"
  , "product_avatar integer,"
  , "product_supplier integer,"
  , "product_max_amount integer not null,"
  -- , "product_total_bought integer not null,"
  , "product_amount_per_crate integer not null,"
  , "product_price_per_crate integer,"
  , "product_art_nr varchar(128)"
  , ")"
  ]

productTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  -- , Field SqlInt4
  -- , Field SqlInt4
  -- , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  -- , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
  ( Field SqlInt4
  , Field SqlText
  -- , Field SqlInt4
  -- , Field SqlInt4
  -- , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  -- , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
productTable = table "product" (
  p9
    ( tableField "product_id"
    , tableField "product_ident"
    -- , tableField "product_price"
    -- , tableField "product_amount"
    -- , tableField "product_vanish"
    , tableField "product_ml"
    , tableField "product_avatar"
    , tableField "product_supplier"
    , tableField "product_max_amount"
    -- , tableField "product_total_bought"
    , tableField "product_amount_per_crate"
    , tableField "product_price_per_crate"
    , tableField "product_art_nr"
    )
  )


productSelect
  :: PGS.Connection
  -> MateHandler [Product]
productSelect conn = do
  bevs <- liftIO $ runSelect conn
    ( keepWhen (\_ -> C.constant True) <<< queryTable productTable
    ) :: MateHandler
        [ ( Int
          , T.Text
          -- , Int
          -- , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, {-i3, i4, i5,-} i6, i7, i8, i9, {-i10,-} i11, i12, i13) -> return $
      Product i1 i2 {-i3 i4 i5-} i6 i7 i8 i9 {-i10-} i11 i12 i13
      )
    bevs


productOverviewSelect
  :: PGS.Connection
  -> MateHandler [ProductOverview]
productOverviewSelect conn = do
  bevs <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        returnA -< (i1, i2, i6, i7, i8, i9, i11, i12, i13)
    ) :: MateHandler
        [ ( Int
          , T.Text
          -- , Int
          -- , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, {-i3, i4, i5,-} i6, i7, i8, i9, {-i10,-} i11, i12, i13) -> do
      amounts <- liftIO $ runSelect conn
        ( proc () -> do
          stuff@(a1, _, _, _, _) <- orderBy (desc (\(_, ts, _, _, _) -> ts))
            (queryTable amountTable) -< ()
          restrict -< C.constant i1 .== a1
          returnA -< stuff
          ) :: MateHandler
              [ ( Int
                , UTCTime
                , Int
                , Int
                , Bool
                )
              ]
      (i3, i4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      i5 <- return $ (\(_, x) -> x) $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      i10 <- return $ snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + max 0 (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
      return $ ProductOverview
        i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13
      )
    bevs


productShortOverviewSelect
  :: PGS.Connection
  -> MateHandler [ProductShortOverview]
productShortOverviewSelect conn = do
  bevs <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        returnA -< (i1, i2, i6, i7, i8, i9, i11, i12, i13)
    ) :: MateHandler
        [ ( Int
          , T.Text
          -- , Int
          -- , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          -- , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, {-i3, i4, i5,-} i6, i7, i8, i9, {-i10,-} i11, i12, i13) -> do
      amounts <- liftIO $ runSelect conn
        ( proc () -> do
          stuff@(a1, _, _, _, _) <- orderBy (desc (\(_, ts, _, _, _) -> ts))
            (queryTable amountTable) -< ()
          restrict -< C.constant i1 .== a1
          returnA -< stuff
          ) :: MateHandler
              [ ( Int
                , UTCTime
                , Int
                , Int
                , Bool
                )
              ]
      (i3, i4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      i5 <- return $ (\(_, x) -> x) $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      i10 <- return $ snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
      return $ ProductShortOverview
        i1 i2 i3 i4 i6 i7
      )
    bevs


-- getProductPrice
--   :: PurchaseDetail
--   -> PGS.Connection
--   -> MateHandler Int
-- getProductPrice (PurchaseDetail bid amount) conn = do
--   when (amount < 0) (
--     throwError $ err406
--       { errBody = "Amounts less or equal zero are not acceptable"
--       }
--     )
--   bevs <- liftIO $ runSelect conn
--     ( keepWhen
--       (\(id_, _, _, _, _, _, _, _, _, _, _) -> id_ .== C.constant bid) <<<
--         queryTable productTable
--     ) :: MateHandler
--         [ ( Int
--           , T.Text
--           -- , Int
--           -- , Int
--           , Int
--           , Int
--           , Maybe Int
--           , Maybe Int
--           , Int
--           , Int
--           , Int
--           , Maybe Int
--           , Maybe T.Text
--           )
--         ]
--   (amount *) <$> head <$> mapM
--     (\(i1, i2, {-i3, i4,-} i5, i6, i7, i8, i9, i10, i11, i12, i13) -> return $
--       i3
--       )
--     bevs


-- checkProductAvailability
--   :: PGS.Connection
--   -> PurchaseDetail
--   -> MateHandler (Maybe Int) -- | returns maybe missing amount
-- checkProductAvailability conn (PurchaseDetail bid amount) = do
--   when (amount <= 0) $
--     throwError $ err406
--       { errBody = "Amounts less or equal zero are not acceptable"
--       }
--   bevs <- liftIO $ runSelect conn
--     ( keepWhen
--       (\(id_, _, _, _, _, _, _, _, _, _, _) -> id_ .== C.constant bid) <<<
--         queryTable productTable
--     ) :: MateHandler
--         [ ( Int
--           , T.Text
--           , Int
--           -- , Int
--           -- , Int
--           , Int
--           , Maybe Int
--           , Maybe Int
--           , Int
--           , Int
--           , Int
--           , Maybe Int
--           , Maybe T.Text
--           )
--         ]
--   realamount <- head <$> mapM
--     (\(i1, i2, i3, {-i4, i5,-} i6, i7, i8, i9, i10, i11, i12, i13) -> return $
--       i4
--       )
--     bevs
--   if realamount < amount
--   then return (Just (amount - realamount))
--   else return Nothing


insertProduct
  :: ProductSubmit
  -> PGS.Connection
  -> MateHandler Int
insertProduct (ProductSubmit ident price ml ava sup max apc ppc artnr) conn =
  fmap head $ liftIO $ runInsert_ conn $ Insert
    { iTable = productTable
    , iRows  =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant ident
      -- , C.constant price
      -- , C.constant (0 :: Int)
      -- , C.constant (0 :: Int)
      , C.constant ml
      , C.constant ava
      , C.constant sup
      , C.constant max
      -- , C.constant (0 :: Int)
      , C.constant apc
      , C.constant ppc
      , C.constant artnr
      )
      ]
    , iReturning = rReturning (\(id, _, _, _, _, _, _, _, _) -> id)
    , iOnConflict = Nothing
    }


-- updateProduct
--   :: Int
--   -> ProductSubmit
--   -> Update Int64
-- updateProduct sid (ProductSubmit ident price ml ava sup max apc ppc artnr) = Update
--   { uTable      = productTable
--   , uUpdateWith = updateEasy (\(id_, _, _, amo, van, _, _, _, _, tot, _, _, _) ->
--       ( id_
--       , C.constant ident
--       , C.constant price
--       , amo
--       , van
--       , C.constant ml
--       , C.constant ava
--       , C.constant sup
--       , C.constant max
--       , tot
--       , C.constant apc
--       , C.constant ppc
--       , C.constant artnr
--       )
--     )
--   , uWhere      =
--     (\(id_, _, _, _, _, _, _, _, _, _, _, _, _) ->
--       id_ .== C.constant sid
--     )
--   , uReturning = rCount
--   }

-- reduceProductAmount
--   :: PurchaseDetail
--   -> Update Int64
-- reduceProductAmount (PurchaseDetail pid amount) = Update
--   { uTable      = productTable
--   , uUpdateWith = updateEasy
--     (\(id_, ident, price, amo, van, ml, ava, sup, max, tot, apc, ppc, artnr) ->
--       ( id_
--       , ident
--       , price
--       , amo - C.constant amount
--       , van
--       , ml
--       , ava
--       , sup
--       , max
--       , tot + C.constant amount
--       , apc
--       , ppc
--       , artnr
--       )
--     )
--   , uWhere      = 
--     (\(id_, _, _, _, _, _, _, _, _, _, _, _, _) ->
--       id_ .== C.constant pid
--     )
--   , uReturning = rCount
--   }
