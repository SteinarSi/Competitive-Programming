import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STArray, getElems, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let ans = runST $ do
            lists <- newArray (1,n) []
            dictator lists 1 xs
            getElems lists

    ans
        & takeWhile (null >>> not)
        & map (reverse >>> map show >>> unwords)
        & unlines
        & putStr

dictator :: forall s. STArray s Int [Int] -> Int -> [Int] -> ST s ()
dictator lists m [] = pure ()
dictator lists m (x:xs) = do
    i <- insert 0 m
    readArray lists i >>= ((x:) >>> writeArray lists i)
    dictator lists (bool m (m+1) (i==m)) xs
  where
    insert :: Int -> Int -> ST s Int
    insert l r
        | r-l <= 1 = pure r
        | otherwise = do
            let mi = (l+r) `div` 2
            y <- readArray lists mi <&> head
            if x > y
                then insert l mi
                else insert mi r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
