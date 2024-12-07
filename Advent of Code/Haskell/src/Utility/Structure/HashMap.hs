module Utility.Structure.HashMap where

import           Control.Arrow (first)
import qualified Data.IntMap.Strict as M

import Utility.Structure.HashSet (Hashable(hash, unhash))

newtype HashMap a = HashMap (M.IntMap a)
    deriving (Eq, Show)

instance Semigroup (HashMap a) where
    (<>) = union

instance Monoid (HashMap a) where
    mempty = empty

empty :: HashMap a
empty = HashMap M.empty

singleton :: Hashable h => h -> a -> HashMap a
singleton i x = HashMap (M.singleton (hash i) x)

fromList :: Hashable h => [(h,a)] -> HashMap a
fromList xs = HashMap (M.fromList (map (first hash) xs))

keys :: Hashable h => HashMap a -> [h]
keys (HashMap m) = map unhash (M.keys m)

elems :: HashMap a -> [a]
elems (HashMap m) = M.elems m

toList :: Hashable h => HashMap a -> [(h,a)]
toList = toAscList

toAscList :: Hashable h => HashMap a -> [(h,a)]
toAscList (HashMap m) = map (first unhash) (M.toAscList m)

toDescList :: Hashable h => HashMap a -> [(h,a)]
toDescList (HashMap m) = map (first unhash) (M.toDescList m)

insert :: Hashable h => h -> a -> HashMap a -> HashMap a
insert i x (HashMap m) = HashMap (M.insert (hash i) x m)

insertWith :: Hashable h => (a -> a -> a) -> h -> a -> HashMap a -> HashMap a
insertWith f i x (HashMap m) = HashMap (M.insertWith f (hash i) x m)

delete :: Hashable h => h -> HashMap a -> HashMap a
delete i (HashMap m) = HashMap (M.delete (hash i) m)

lookup :: Hashable h => h -> HashMap a -> Maybe a
lookup i (HashMap m) = M.lookup (hash i) m

member :: Hashable h => h -> HashMap a -> Bool
member i (HashMap m) = M.member (hash i) m

notMember :: Hashable h => h -> HashMap a -> Bool
notMember i hs = not (member i hs)

size :: HashMap a -> Int
size (HashMap m) = M.size m

union :: HashMap a -> HashMap a -> HashMap a
union (HashMap a) (HashMap b) = HashMap (a `M.union` b)

intersection :: HashMap a -> HashMap a -> HashMap a
intersection (HashMap a) (HashMap b) = HashMap (a `M.intersection` b)

difference :: HashMap a -> HashMap a -> HashMap a
difference (HashMap a) (HashMap b) = HashMap (a `M.difference` b)
