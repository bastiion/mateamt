{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model.Product where

import Data.Text as T hiding (head, foldl)
import Data.Time.Clock (UTCTime)
import Data.Profunctor.Product (p9)

import Control.Monad.IO.Class (liftIO)

import Control.Arrow

import qualified Database.PostgreSQL.Simple as PGS

import qualified Data.Profunctor as P

import Opaleye as O
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
  :: ProductRefine
  -> PGS.Connection
  -> MateHandler [ProductOverview]
productOverviewSelect refine conn = do
  prods <- liftIO $ runSelect conn (produceProductOverviews refine)
  mapM
    (generateProductOverview conn)
    prods

produceProductOverviews
  :: ProductRefine
  -> Select
      ( Column PGInt4
      , Column PGText
      , Column PGInt4
      , Column (Nullable PGInt4)
      , Column (Nullable PGInt4)
      , Column PGInt4
      , Column PGInt4
      , Column (Nullable PGInt4)
      , Column (Nullable PGText)
      , Column PGInt4
      , Column PGInt4
      )
produceProductOverviews refine =
  ( proc () -> do
      (p, i2, i6, i7, i8, i9, i11, i12, i13, a3, a4)
        <- orderBy (asc (\(_, a2, _, _, _, _, _, _, _, _, _) -> a2)) (leftJoinF
             (\(pid, pi2, pi6, pi7, pi8, pi9, pi11, pi12, pi13)
               (_, _, ai3, ai4, _) ->
                 (pid, pi2, pi6, pi7, pi8, pi9, pi11, pi12, pi13, ai3, ai4)
               )
             (\_ ->
               ( (C.constant (0 :: Int) :: Column PGInt4)
               , (C.constant ("ERROR PRODUCT" :: T.Text) :: Column PGText)
               , (C.constant (0 :: Int) :: Column PGInt4)
               , (C.constant (Just (0 :: Int)) :: Column (Nullable PGInt4))
               , (C.constant (Just (0 :: Int)) :: Column (Nullable PGInt4))
               , (C.constant (0 :: Int) :: Column PGInt4)
               , (C.constant (0 :: Int) :: Column PGInt4)
               , (C.constant (Just (0 :: Int)) :: Column (Nullable PGInt4))
               , (C.constant (Just ("" :: T.Text)) :: Column (Nullable PGText))
               , (C.constant (0 :: Int) :: Column PGInt4)
               , (C.constant (0 :: Int) :: Column PGInt4)
               )
               )
             (\(pid, _, _, _, _, _, _, _, _)
               (aid, _, _, _, _) ->
               pid .== aid
               )
             (selectTable productTable)
             (joinF
               (\(_, _, a3, a4, a5) (b1, b2) ->
                 (b1, b2, a3, a4, a5)
                 )
               (\(a1, a2, _, _, _) (b1, b2) ->
                 (a1 .== b1) .&& (a2 .== b2)
                 )
               (selectTable amountTable)
               (aggregate
                 ((,)
                   <$> P.lmap fst O.groupBy
                   <*> P.lmap snd O.max
                   )
                 (arr (\(a, b, _, _, _) -> (a, b)) <<< selectTable amountTable))
               )) -< ()
            -- <<< arr (\_ -> (selectTable productTable, selectTable amountTable)) -< ()
      restrict -< case refine of
        AllProducts -> C.constant True
        AvailableProducts -> a3 ./= (C.constant (0 :: Int) :: Column PGInt4)
        DepletedProducts -> a3 .== (C.constant (0 :: Int) :: Column PGInt4)
      returnA -< (p, i2, i6, i7, i8, i9, i11, i12, i13, a3, a4)
  )

queryAmounts
  :: PGS.Connection
  -> Int
  -> IO [(Int, UTCTime, Int, Int, Bool)]
queryAmounts conn pid = runSelect conn $ proc () -> do
  stuff@(a1, _, _, _, _) <- orderBy (desc (\(_, ts, _, _, _) -> ts))
    (queryTable amountTable) -< ()
  restrict -< C.constant pid .== a1
  returnA -< stuff

generateProductOverview
  :: PGS.Connection
  -> ( Int
     , Text
     , Int
     , Maybe Int
     , Maybe Int
     , Int
     , Int
     , Maybe Int
     , Maybe Text
     , Int
     , Int
     )
  -> MateHandler ProductOverview
generateProductOverview conn (i1, i2, i3, i4, i5, i6, i7, i8, i9, a3, a4) = do
  amounts <- liftIO $ queryAmounts conn i1
  let ii5 = snd $
        foldl
          (\(bef, van) (_, _, amo, _, ver) ->
            if ver
            then (amo, if amo < bef then van + (bef - amo) else van)
            else (amo, van)
            )
          (0, 0)
          (Prelude.reverse amounts)
      ii10 = snd $ foldl (\(bef, tot) (_, _, amo, _, ver) ->
        if ver
        then (amo, tot)
        else (amo, tot + Prelude.max 0 (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
  return $ ProductOverview
    i1 i2 a4 a3 ii5 i3 i4 i5 i6 ii10 i7 i8 i9

productOverviewSelectSingle
  :: Int
  -> PGS.Connection
  -> MateHandler ProductOverview
productOverviewSelectSingle pid conn = do
  prods <- liftIO $ runSelect conn (produceProductOverviews AllProducts)
  head <$> mapM
    (generateProductOverview conn)
    (Prelude.filter (\(p, _, _, _, _, _, _, _, _, _, _) -> p == pid) prods)


productShortOverviewSelect
  :: ProductRefine
  -> PGS.Connection
  -> MateHandler [ProductShortOverview]
productShortOverviewSelect refine conn = do
  prods <- liftIO $ runSelect conn (produceProductOverviews refine)
    :: MateHandler [
       ( Int
       , Text
       , Int
       , Maybe Int
       , Maybe Int
       , Int
       , Int
       , Maybe Int
       , Maybe Text
       , Int
       , Int
       )]
  mapM
    (\(i1, i2, i3, i4, _, _, _, _, _, a3, a4) -> do
      return $ ProductShortOverview
        i1 i2 a4 a3 i3 i4
      )
    prods


insertProduct
  :: ProductSubmit
  -> PGS.Connection
  -> MateHandler Int
insertProduct (ProductSubmit ident _ ml ava sup maxi apc ppc artnr) conn =
  fmap head $ liftIO $ runInsert_ conn $ Insert
    { iTable = productTable
    , iRows  =
      [
      ( C.constant (Nothing :: Maybe Int)
      , C.constant ident
      , C.constant ml
      , C.constant ava
      , C.constant sup
      , C.constant maxi
      , C.constant apc
      , C.constant ppc
      , C.constant artnr
      )
      ]
    , iReturning = rReturning (\(id_, _, _, _, _, _, _, _, _) -> id_)
    , iOnConflict = Nothing
    }
