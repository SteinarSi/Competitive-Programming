import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Tuple    (swap)

main :: IO ()
main = do
    [n,m] <- getLine <&> words
    print $ solve (read n) (bool inn out (m=="out"))

solve :: Int -> ([Int] -> [Int]) -> Int
solve n f = iterate f (f xs)
        & takeWhile (/=xs)
        & length
        & succ
  where
    xs = [1..n]

inn :: [a] -> [a]
inn xs = splitAt (length xs `div` 2) xs
    & swap
    & uncurry everyOther

out :: [a] -> [a]
out xs = splitAt ((length xs + 1) `div` 2) xs
    & uncurry everyOther

everyOther :: [a] -> [a] -> [a]
everyOther [] ys     = ys
everyOther (x:xs) ys = x : everyOther ys xs
