import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM, when)
import           Control.Monad.ST (ST)
import           Data.Array.Base  (STUArray, UArray, newArray, readArray,
                                   writeArray, (!))
import           Data.Array.ST    (runSTUArray)
import           Data.Char        (digitToInt)
import           Data.Functor     ((<&>))
import qualified Data.IntSet      as S
import           Data.List        (delete)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

bound :: Int
bound = 9999999

solve :: String -> Int
solve = combinations 0 >>> S.toList >>> filter (prime !) >>> length

combinations :: Int -> String -> S.IntSet
combinations r "" = S.singleton r
combinations r xs = S.insert r $ S.unions (map (\x -> combinations (10*r + digitToInt x) (delete x xs)) xs)

prime :: UArray Int Bool
prime = runSTUArray $ do
    sieve <- newArray (0,bound) True :: ST s (STUArray s Int Bool)
    writeArray sieve 0 False
    writeArray sieve 1 False
    filterM (\p -> do
            s <- readArray sieve p
            when s (mapM_ (flip (writeArray sieve) False) [p+p,p+p+p .. bound])
            pure s
        ) [2..bound]
    pure sieve
