module Utility.Structure.HashSet where

import qualified Data.IntSet as S

class Hashable h where
    hash   :: h -> Int
    unhash :: Int -> h

instance Hashable Int where
    hash = id
    unhash = id

instance Hashable (Int,Int) where
    hash (a,b) = 100000 * a + b
    unhash = (`quotRem` 100000)

instance Hashable Bool where
    hash False = 0
    hash True  = 1
    unhash 0 = False
    unhash 1 = True

newtype HashSet = HashSet S.IntSet
    deriving (Eq, Show)

instance Semigroup HashSet where
    (<>) = union

instance Monoid HashSet where
    mempty = empty

empty :: HashSet
empty = HashSet S.empty

singleton :: Hashable h => h -> HashSet
singleton x = HashSet (S.singleton (hash x))

fromList :: Hashable h => [h] -> HashSet
fromList xs = HashSet (S.fromList (map hash xs))

toList :: Hashable h => HashSet -> [h]
toList = toAscList

toAscList :: Hashable h => HashSet -> [h]
toAscList (HashSet set) = map unhash (S.toAscList set)

toDescList :: Hashable h => HashSet -> [h]
toDescList (HashSet set) = map unhash (S.toDescList set)

insert :: Hashable h => h -> HashSet -> HashSet
insert x (HashSet set) = HashSet (S.insert (hash x) set)

delete :: Hashable h => h -> HashSet -> HashSet
delete x (HashSet set) = HashSet (S.delete (hash x) set)

member :: Hashable h => h -> HashSet -> Bool
member x (HashSet set) = S.member (hash x) set

notMember :: Hashable h => h -> HashSet -> Bool
notMember x hs = not (member x hs)

size :: HashSet -> Int
size (HashSet set) = S.size set

union :: HashSet -> HashSet -> HashSet
union (HashSet a) (HashSet b) = HashSet (a `S.union` b)

unions :: [HashSet] -> HashSet
unions [] = empty
unions [x] = x
unions (x:y:xs) = unions (union x y : xs)

intersection :: HashSet -> HashSet -> HashSet
intersection (HashSet a) (HashSet b) = HashSet (a `S.intersection` b)

difference :: HashSet -> HashSet -> HashSet
difference (HashSet a) (HashSet b) = HashSet (a `S.difference` b)
