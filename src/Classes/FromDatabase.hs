{-# LANGUAGE TypeFamilies #-}
module Classes.FromDatabase where

class FromDatabase a where

  type OutTuple a :: *

  fromDatabase :: OutTuple a -> a
