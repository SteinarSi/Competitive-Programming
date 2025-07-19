import           Control.Arrow         ((>>>))
import           Control.Monad         (when, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (Ix (..), MArray, STArray, getElems,
                                        newArray, readArray, writeArray)
import           Data.Bits             (shiftL)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let p = round (logBase 2 (fromIntegral n))
    solve n xs
        & map show
        & unwords
        & putStrLn

solve :: Int -> [Int] -> [Int]
solve n xs | not (validate count) = [-1]
           | otherwise            = distribute (drop 1 count) (head count)
  where
    p = round (logBase 2 (fromIntegral n))

    distribute :: [[Int]] -> [Int] -> [Int]
    distribute []       = id
    distribute (xs:xss) = zipWith (\x y -> [min x y, max x y]) xs >>> concat >>> distribute xss

    count :: [[Int]]
    count = runST $ do
        ret <- newArray (0,p) [] :: ST s (STArray s Int [Int])
        zipWithM_ (\i x -> when (inRange (0,p) x) (modifyArray ret x (i:))) [1..] xs
        getElems ret <&> (map reverse >>> reverse)

    validate :: [[Int]] -> Bool
    validate (c:cs) = length c == 1 && and (zipWith (\s c -> length c == 1 `shiftL` s) [0..p] cs)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
