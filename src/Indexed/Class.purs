module Indexed.Class where

import Prelude

import Data.Tuple (Tuple, fst)

class Indexed k v where
  getIndex :: v -> k

instance Indexed k (Tuple k v) where
  getIndex = fst

instance Indexed Unit Unit where
  getIndex = identity