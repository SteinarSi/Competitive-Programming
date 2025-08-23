import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))
import           Data.List             (delete, elemIndex, sortOn)
import           Data.Ord              (Down (Down))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> map parseCard >>> sortOn Down)
        >>> split
        >>> map (uncurry nondet >>> show)
        >>> unlines
        >>> putStr
    )

nondet :: [Int] -> [Int] -> Int
nondet ys [] = 0
nondet [] xs = 0
nondet (y:ys) (x:xs) = case compare y x of
        LT -> 2 + pick
        EQ -> max (1 + pick) skip
        GT -> skip
    where
        pick = nondet ys xs
        skip = nondet ys (x:xs)

split :: [[Int]] -> [([Int],[Int])]
split []          = []
split (xs:ys:xss) = (xs,ys) : split xss

parseCard :: Char -> Int
parseCard c = maybe (digitToInt c) (10+) (elemIndex c "TJQKA")
