module Data.IxMap
  ( IxMap
  , delete
  , empty
  , fromMap
  , insert
  , lookup
  , map
  , toMap
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (lens)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Indexed.Class (class Indexed, getIndex)

newtype IxMap k v = IxMap (Map k v)

toMap :: forall k v. IxMap k v -> Map k v
toMap (IxMap mp) = mp

fromMap :: forall k v. Eq v => Eq k => Indexed k v => Map k v -> Maybe (IxMap k v)
fromMap mp
  | Map.toUnfoldable mp
      # Array.all isValidKeyValTuple = Just $ IxMap mp
fromMap _ = Nothing

map :: forall k k' v v'. Ord k' => Indexed k' v' => (v -> v') -> IxMap k v -> IxMap k' v'
map f (IxMap mp) = Map.values mp
  <#> f >>> toKeyValTuple
  # Map.fromFoldable
  # IxMap

toKeyValTuple :: forall k v. Indexed k v => v -> Tuple k v
toKeyValTuple val = getIndex val /\ val

isValidKeyValTuple :: forall k v. Eq k => Eq v => Indexed k v => Tuple k v -> Boolean
isValidKeyValTuple tpl@(_ /\ val) = tpl == toKeyValTuple val

empty :: forall k v. IxMap k v
empty = IxMap Map.empty

insert :: forall k v. Ord k => Indexed k v => v -> IxMap k v -> IxMap k v
insert val (IxMap mp) = IxMap $ Map.insert (getIndex val) val mp

lookup :: forall k v. Ord k => k -> IxMap k v -> Maybe v
lookup key (IxMap mp) = Map.lookup key mp

delete :: forall k v. Ord k => k -> IxMap k v -> IxMap k v
delete key (IxMap mp) = IxMap $ Map.delete key mp

instance (Ord k, Indexed k v) => At (IxMap k v) k v where
  at k =
    lens (lookup k) \m ->
      maybe' (\_ -> delete k m) \v -> insert v m

instance (Ord k, Indexed k v) => Index (IxMap k v) k v where
  ix k = affineTraversal set pre
    where
    set :: IxMap k v -> v -> IxMap k v
    set s b = insert b s

    pre :: IxMap k v -> Either (IxMap k v) v
    pre s = maybe (Left s) Right $ lookup k s
