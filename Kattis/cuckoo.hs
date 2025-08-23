import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, listArray, (!))
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> (head &&& last))
        >>> split
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int) -> Array Int (Int,Int) -> String
solve (n,m) xs = runST $ do
    table  <- newArray (0,n-1) 0
    mapM (insert table) [1..m] <&> (and >>> bool "rehash necessary" "successful hashing")
  where
    insert :: STUArray s Int Int -> Int -> ST s Bool
    insert table d = do
        let (h1,h2) = xs ! d
        a <- readArray table h1
        b <- readArray table h2
        case (a,b) of
            (0,_) -> writeArray table h1 d >> pure True
            (_,0) -> writeArray table h2 d >> pure True
            (r,_) -> writeArray table h1 d >> evict table (d,h1) (r,other r h1)

    evict :: STUArray s Int Int -> (Int,Int) -> (Int,Int) -> ST s Bool
    evict table start (d,f) | start == (d,f) = pure False
                            | otherwise = do
        r <- readArray table f
        writeArray table f d
        if r == 0
            then pure True
            else evict table start (r, other r f)

    other :: Int -> Int -> Int
    other r h | h == p    = q
              | otherwise = p
      where (p,q) = xs ! r

split :: [(Int,Int)] -> [((Int,Int),Array Int (Int,Int))]
split [] = []
split ((m,n):xs) = splitAt m xs
        & (listArray (1,m) >>> ((n,m),)) *** split
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
