module Utility.Structure.HashSet where

import qualified Data.IntSet as S

class Hashable a where
    hash   :: a -> Int
    unhash :: Int -> a

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

singleton :: Hashable a => a -> HashSet
singleton x = HashSet (S.singleton (hash x))

fromList :: Hashable a => [a] -> HashSet
fromList xs = HashSet (S.fromList (map hash xs))

insert :: Hashable a => a -> HashSet -> HashSet
insert x (HashSet set) = HashSet (S.insert (hash x) set)

delete :: Hashable a => a -> HashSet -> HashSet
delete x (HashSet set) = HashSet (S.delete (hash x) set)

member :: Hashable a => a -> HashSet -> Bool
member x (HashSet set) = S.member (hash x) set

notMember :: Hashable a => a -> HashSet -> Bool
notMember x hs = not (member x hs)

size :: HashSet -> Int
size (HashSet set) = S.size set

union :: HashSet -> HashSet -> HashSet
union (HashSet a) (HashSet b) = HashSet (a `S.union` b)

intersection :: HashSet -> HashSet -> HashSet
intersection (HashSet a) (HashSet b) = HashSet (a `S.intersection` b)

difference :: HashSet -> HashSet -> HashSet
difference (HashSet a) (HashSet b) = HashSet (a `S.difference` b)
