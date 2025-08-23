import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import           Data.Array            (Array, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words 
            >>> map readInt 
            >>> (\[i,n,m,k] -> C.pack (show i <> " " <> show (solve n m k))))
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> Int -> Int -> Int
solve n m k = dp ! n
    where
        valid :: Int -> Bool
        valid x = x `mod` k /= m

        dp :: Array Int Int
        dp = listArray (1,n) $ map f [1..n]

        f :: Int -> Int
        f x = bool 0 1 (valid x) + sum [dp ! (x-i) | i <- [1..x-1], valid i]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
