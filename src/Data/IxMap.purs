module Data.IxMap
  ( IxMap
  , delete
  , empty
  , fromMap
  , insert
  , lookup
  , map
  , map'
  , toMap
  )
  where

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
import Data.Tuple.Nested ((/\))

data IxMap k v = IxMap (v -> k) (Map k v)

toMap :: forall k v. IxMap k v -> Map k v
toMap (IxMap _ mp) = mp

fromMap :: forall k v. Eq v => Eq k => (v -> k) -> Map k v -> Maybe (IxMap k v)
fromMap ixfn mp
  | Map.toUnfoldable mp
      # Array.all (\tpl@(_ /\ val) -> tpl == (ixfn val /\ val)) = Just $ IxMap ixfn mp
fromMap _ _ = Nothing

map' :: forall k v. Ord k => (v -> v) -> IxMap k v -> IxMap k v
map' f mp@(IxMap ixfn _) = map ixfn f mp

map :: forall k k' v v'. Ord k' => (v' -> k') -> (v -> v') -> IxMap k v -> IxMap k' v'
map ixfn f (IxMap _ mp) = Map.values mp
  <#> f >>> (\x -> ixfn x /\ x)
  # Map.fromFoldable
  # IxMap ixfn

empty :: forall k v. (v -> k) -> IxMap k v
empty ixfn = IxMap ixfn Map.empty

insert :: forall k v. Ord k => v -> IxMap k v -> IxMap k v
insert val (IxMap ixfn mp) = IxMap ixfn $ Map.insert (ixfn val) val mp

lookup :: forall k v. Ord k => k -> IxMap k v -> Maybe v
lookup key (IxMap _ mp) = Map.lookup key mp

delete :: forall k v. Ord k => k -> IxMap k v -> IxMap k v
delete key (IxMap ixfn mp) = IxMap ixfn $ Map.delete key mp

instance (Ord k) => At (IxMap k v) k v where
  at k =
    lens (lookup k) \m ->
      maybe' (\_ -> delete k m) \v -> insert v m

instance (Ord k) => Index (IxMap k v) k v where
  ix k = affineTraversal set pre
    where
    set :: IxMap k v -> v -> IxMap k v
    set s b = insert b s

    pre :: IxMap k v -> Either (IxMap k v) v
    pre s = maybe (Left s) Right $ lookup k s
