import           Control.Arrow (first, (>>>))
import           Data.Bool     (bool)
import           Data.Char     (digitToInt)
import qualified Data.IntSet   as S

main :: IO ()
main = getLine >> getLine >>= (
            map parse
        >>> search
        >>> S.member 67
        >>> bool "no" "yes"
        >>> putStrLn
    )

parse :: Char -> Int
parse 'A' = 1
parse 'T' = 10
parse 'J' = 11
parse 'Q' = 12
parse 'K' = 13
parse  x  = digitToInt x

search :: [Int] -> S.IntSet
search [] = S.empty
search [x] = S.singleton x
search xs = S.fromList $ do
    (ys,zs) <- partitions xs
    y <- S.toList (search ys)
    z <- S.toList (search zs)
    [y+z, y*z]

partitions :: [a] -> [([a],[a])]
partitions [] = error "bruh"
partitions (y:ys) = map (first (y:)) (drop 1 (ps ys))
  where
    ps :: [a] -> [([a],[a])]
    ps []     = []
    ps [x]    = [([x],[]),([],[x])]
    ps (x:xs) = concatMap (\(ys,zs) -> [(x:ys,zs),(ys,x:zs)]) (ps xs)
