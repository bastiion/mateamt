{-# LANGUAGE TypeFamilies #-}
module Classes.ToDatabase where

class ToDatabase a where

  type InTuple a :: *

  toDatabase :: a -> InTuple a
