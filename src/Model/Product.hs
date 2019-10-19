{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Model.Product where

import Data.Text as T hiding (head, foldl)
import Data.Time.Clock (UTCTime)
import Data.Profunctor.Product (p9)

import Control.Monad.IO.Class (liftIO)

import Control.Arrow ((<<<), returnA, arr)

import qualified Database.PostgreSQL.Simple as PGS

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
  :: ProductRefine
  -> PGS.Connection
  -> MateHandler [ProductOverview]
productOverviewSelect refine conn = do
  prods <- liftIO $ runSelect conn
    ( proc () -> do
        -- (a1, _, a3, _, _)
        --     <- arr
        --       (\(arrdata, pid) -> (limit 1 (
        --         orderBy (desc (\(_, ts, _, _, _) -> ts)) (
        --           keepWhen (\(aid, _, _, _, _) -> pid .== aid) <<< arrdata)))) <<<
        --             (arr (\pid -> (selectTable amountTable, pid))) -< pid
        (pid, i2, i6, i7, i8, i9, i11, i12, i13, a3) ::
          ( Column PGInt4
          , Column PGText
          , Column PGInt4
          , Column (Nullable PGInt4)
          , Column (Nullable PGInt4)
          , Column (PGInt4)
          , Column (PGInt4)
          , Column (Nullable SqlInt4)
          , Column (Nullable SqlText)
          , Column PGInt4
          ) <-
          arr (\((p, i2, i6, i7, i8, i9, i11, i12, i13), (_, _, a3, _, _)) ->
            (p, i2, i6, i7, i8, i9, i11, i12, i13, a3))
            <<< leftJoin
              (selectTable productTable)
              (selectTable amountTable)
              (\((pid, _, _, _, _, _, _, _, _), (aid, _, _, _, _)) ->
                pid .== aid
                ) -< ()
        restrict -< case refine of
          AllProducts -> C.constant True
          AvailableProducts -> a3 ./= C.constant (0 :: Int)
          DepletedProducts -> a3 .== C.constant (0 :: Int)
        returnA -< (pid, i2, i6, i7, i8, i9, i11, i12, i13)
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
    (generateProductOverview conn)
    prods

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
  -> (Int, Text, Int, Maybe Int, Maybe Int, Int, Int, Maybe Int, Maybe Text)
  -> MateHandler ProductOverview
generateProductOverview conn (i1, i2, i3, i4, i5, i6, i7, i8, i9) = do
  amounts <- liftIO $ queryAmounts conn i1
  (ii3, ii4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
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
        else (amo, tot + max 0 (bef - amo))
        )
        (0, 0)
        (Prelude.reverse amounts)
  return $ ProductOverview
    i1 i2 ii3 ii4 ii5 i3 i4 i5 i6 ii10 i7 i8 i9

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
    (generateProductOverview conn)
    prods


productShortOverviewSelect
  :: ProductRefine
  -> PGS.Connection
  -> MateHandler [ProductShortOverview]
productShortOverviewSelect refine conn = do
  prods <- liftIO $ runSelect conn
    ( proc () -> do
        (i1, i2, i6, i7, i8, i9, i11, i12, i13) <- queryTable productTable -< ()
        (a1, _, a3, _, _) <-
          limit 1 (
            orderBy (desc (\(_, ts, _, _, _) -> ts)) (queryTable amountTable))
              -< ()
        restrict -< a1 .== i1
        restrict -< case refine of
          AllProducts -> C.constant True
          AvailableProducts -> a3 ./= C.constant (0 :: Int)
          DepletedProducts -> a3 .== C.constant (0 :: Int)
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
    (\(i1, i2, i3, i4, _, _, _, _, _) -> do
      amounts <- liftIO $ queryAmounts conn i1
      (ii3, ii4) <- return $ (\(_, _, y, x, _) -> (x, y)) $ head amounts
      return $ ProductShortOverview
        i1 i2 ii3 ii4 i3 i4
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
