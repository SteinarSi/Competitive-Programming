import           Control.Arrow         ((>>>), (&&&))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (m,s):ms <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    print $ runST $ do
            seen <- newArray (0,m-1) False
            search seen m ms 0 [s]

search :: STUArray s Int Bool -> Int -> [(Int,Int)] -> Int -> [Int] -> ST s Int
search _    _ _  _ [] = pure (-1)
search seen m ms r xs | 0 `elem` xs = pure r
                      | otherwise = [ (x*a + b) `mod` m | x <- xs, (a,b) <- ms]
            & filterM (\x -> readArray seen x >>= \s -> if s then pure False else writeArray seen x True >> pure True)
            >>= search seen m ms (r+1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
