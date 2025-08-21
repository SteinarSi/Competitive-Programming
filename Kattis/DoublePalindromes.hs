import           Control.Arrow ((>>>))
import           Data.Bits     (shiftL, (.|.))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (sort)
import           Text.Printf   (printf)

main :: IO ()
main = do
    n <- getLine <&> read
    takeWhile (<n) hardcoded
        & length
        & print

palindrome :: String -> Bool
palindrome xs = xs == reverse xs

hardcoded :: [Int]
hardcoded = [0,1,3,5,7,9,33,99,313,585,717,7447,9009,15351,32223,39993,53235,53835,73737,585585,1758571,1934391,1979791,3129213,5071705,5259525,5841485,13500531,719848917,910373019,939474939,1290880921,7451111547,10050905001,18462126481,32479297423,75015151057,110948849011,136525525631]

doublePalindromes :: [Int]
doublePalindromes = binaryPalindromes
    & sort
    & filter (show >>> palindrome)

binaryPalindromes :: [Int]
binaryPalindromes = recursive 3 [(0,1),(1,1)] <> drop 1 (recursive 4 [(0,2),(3,2)])
  where
    recursive :: Int -> [(Int,Int)] -> [Int]
    recursive l prev
        | null next = map fst prev
        | otherwise = recursive (l+2) (prev <> next)
      where
        next = map (\(p,l') -> ((p `shiftL` ((l-l') `div` 2)) .|. (1 `shiftL` (l-1)) .|. 1, l)) prev
            & filter (fst >>> (<=10^12))
