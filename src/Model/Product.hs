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
  [ "CREATE TABLE IF NOT EXISTS \"product\" ("
  , "product_id               SERIAL  PRIMARY KEY,"
  , "product_ident            TEXT    NOT NULL,"
  , "product_ml               INTEGER NOT NULL,"
  , "product_avatar           INTEGER,"
  , "product_supplier         INTEGER,"
  , "product_max_amount       INTEGER NOT NULL,"
  , "product_amount_per_crate INTEGER NOT NULL,"
  , "product_price_per_crate  INTEGER,"
  , "product_art_nr           TEXT"
  , ")"
  ]

productTable :: Table
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , FieldNullable SqlInt4
  , FieldNullable SqlText
  )
productTable = table "product" (
  p9
    ( tableField "product_id"
    , tableField "product_ident"
    , tableField "product_ml"
    , tableField "product_avatar"
    , tableField "product_supplier"
    , tableField "product_max_amount"
    , tableField "product_amount_per_crate"
    , tableField "product_price_per_crate"
    , tableField "product_art_nr"
    )
  )


productSelect
  :: PGS.Connection
  -> MateHandler [Product]
productSelect conn = do
  prods <- liftIO $ runSelect conn
    ( keepWhen (\_ -> C.constant True) <<< queryTable productTable
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> return $
      Product i1 i2 i3 i4 i5 i6 i7 i8 i9
      )
    prods


productOverviewSelect
  :: PGS.Connection
  -> MateHandler [ProductOverview]
productOverviewSelect conn = do
  prods <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        returnA -< (i1, i2, i6, i7, i8, i9, i11, i12, i13)
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> do
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
      (ii3, ii4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      ii5 <- return $ (\(_, x) -> x) $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      ii10 <- return $ snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + max 0 (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
      return $ ProductOverview
        i1 i2 ii3 ii4 ii5 i3 i4 i5 i6 ii10 i7 i8 i9
      )
    prods


productOverviewSelectSingle
  :: Int
  -> PGS.Connection
  -> MateHandler ProductOverview
productOverviewSelectSingle pid conn = do
  prods <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        restrict -< C.constant pid .== i1
        returnA -< (i1, i2, i6, i7, i8, i9, i11, i12, i13)
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  head <$> mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> do
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
      (ii3, ii4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      ii5 <- return $ (\(_, x) -> x) $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      ii10 <- return $ snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + max 0 (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
      return $ ProductOverview
        i1 i2 ii3 ii4 ii5 i3 i4 i5 i6 ii10 i7 i8 i9
      )
    prods


productShortOverviewSelect
  :: PGS.Connection
  -> MateHandler [ProductShortOverview]
productShortOverviewSelect conn = do
  prods <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        returnA -< (i1, i2, i6, i7, i8, i9, i11, i12, i13)
    ) :: MateHandler
        [ ( Int
          , T.Text
          , Int
          , Maybe Int
          , Maybe Int
          , Int
          , Int
          , Maybe Int
          , Maybe T.Text
          )
        ]
  mapM
    (\(i1, i2, i3, i4, i5, i6, i7, i8, i9) -> do
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
      (ii3, ii4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      ii5 <- return $ (\(_, x) -> x) $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      ii10 <- return $ snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
      return $ ProductShortOverview
        i1 i2 ii3 ii4 i3 i4
      )
    prods


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
      , C.constant ml
      , C.constant ava
      , C.constant sup
      , C.constant max
      , C.constant apc
      , C.constant ppc
      , C.constant artnr
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _) -> id_)
    , iOnConflict = Nothing
    }
