import           Control.Arrow         (first, second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (..))

main :: IO ()
main = do
    (n,k):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> (head &&& last)))

    let left  = filter (fst >>> (<0)) xs & map (first negate)
        right = filter (fst >>> (>=0)) xs

    print (solve k left + solve k right)

solve :: Int -> [(Int,Int)] -> Int
solve k = sortOn Down >>> solulu k Nothing

solulu :: Int -> Maybe (Int,Int) -> [(Int,Int)] -> Int
solulu k Nothing [] = 0
solulu k (Just (x,t)) [] = 2 * x * ((t+k-1) `div` k)
solulu k Nothing ((x,t):xs) = let (q,r) = quotRem t k
                              in  2 * x * q + solulu k (Just (x,r)) xs
solulu k (Just (x1,t1)) ((x2,t2):xs) = 2 * x2 * q + case compare (t1+r) k of
                                                        LT -> solulu k (Just (x1,t1+r)) xs
                                                        EQ -> 2 * x1 + solulu k Nothing xs
                                                        GT -> 2 * x1 + solulu k (Just (x2,(r+t1)`mod`k)) xs
    where
        (q,r) = quotRem t2 k

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
