import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import qualified Data.IntSet           as S

main :: IO ()
main = C.getLine >>= (
            C.group
        >>> map (C.head >>> ord)
        >>> solve
        >>> print
    )

solve :: [Int] -> Int
solve []     = 0
solve (x:xs) = solve' S.empty xs + solve xs
  where
    solve' :: S.IntSet -> [Int] -> Int
    solve' between []        = 0
    solve' between (y:ys)
        | y == x             = 0
        | S.member y between =     solve' (S.insert y between) ys
        | otherwise          = 1 + solve' (S.insert y between) ys
