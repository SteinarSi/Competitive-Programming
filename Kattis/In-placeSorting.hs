import           Control.Applicative   ((<|>))
import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> solve minBound
        >>> maybe "impossible\n" (map show >>> ("possible":) >>> unlines)
        >>> putStr
    )

solve :: Int -> [Int] -> Maybe [Int]
solve _ [] = Just []
solve p (x:xs) = smallest x (zip [n-1,n-2..] x') >>= \q -> (q:) <$> solve q xs
  where
    x' = show x
    n = length x'

    smallest :: Int -> [(Int,Char)] -> Maybe Int
    smallest c [] | c >= p = Just c
                  | otherwise = Nothing
    smallest c (y:ys) = case y of
        (i,_) | c + threes ! i < p -> Nothing
        (i,'9') -> smallest (c - 3 * 10^i) ys <|> smallest c ys
        (i,'6') -> smallest c ys <|> smallest (c + 3 * 10^i) ys
        _       -> smallest c ys

threes :: UArray Int Int
threes = listArray (0,18) (iterate (\x -> 10*x + 3) 3)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
