import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, array, bounds, elems, listArray,
                                        (!))
import           Data.Array.ST         (MArray (..), STUArray, getElems,
                                        readArray, runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix (..))
import           Data.List             (sortOn, tails)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    C.putStrLn (solve (parse n xs))

solve :: Array Int [Int] -> C.ByteString
solve graph = runST $ do
        labels <- newArray (bounds graph) 0
        let decomp = decompose graph
            (a,b) = splitAt (length decomp) primes
        zipWithM_ (writeArray labels) decomp a
        label labels 1 b 1
        getElems labels <&> (map show >>> unwords >>> C.pack)

    where
        h = height graph

        label :: STUArray s Int Int -> Int -> [Int] -> Int -> ST s ()
        label labels curr ps@(p:_) u = do
            r <- readArray labels u

            let (l,ps') | u == 1    = (1,r:ps)
                        | r == 0    = (curr * p, ps)
                        | otherwise = (curr * r, r:ps)
            writeArray labels u l

            graph ! u
                & sortOn ((h!) >>> negate)
                & zipWithM_ (label labels l) (tails ps')

decompose :: Array Int [Int] -> [Int]
decompose graph = sortOn ((h!) >>> negate) (decomp 1 1)
    where
        h = height graph

        decomp :: Int -> Int -> [Int]
        decomp s u = case sortOn ((h!) >>> negate) (graph ! u) of
                []     -> [s]
                (v:vs) -> decomp s v <> concatMap (\v' -> decomp v' v') vs

height :: Array Int [Int] -> Array Int Int
height graph = h
    where
        rng = bounds graph

        h :: Array Int Int
        h = listArray rng (map f (range rng))

        f :: Int -> Int
        f u | null children = 1
            | otherwise     = 1 + maximum children
            where
                children = map (h !) (graph ! u)

parse :: Int -> [[Int]] -> Array Int [Int]
parse n xs = array (1,n) (orphan 1 1)
    where
        graph = runSTArray $ do
            g <- newArray (1,n) []
            forM_ xs $ \[a,b] -> do
                modifyArray g a (b:)
                modifyArray g b (a:)
            pure g

        orphan :: Int -> Int -> [(Int,[Int])]
        orphan p u = let children = filter (p/=) (graph ! u)
                     in  (u, children) : concatMap (orphan u) children

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997]
