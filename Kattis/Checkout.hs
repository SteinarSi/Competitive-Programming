import           Control.Arrow ((>>>))
import           Data.Char     (toLower)
import           Data.Function ((&))
import           Data.Maybe    (listToMaybe)

main :: IO ()
main = getContents >>= (
            read
        >>> play 3 []
        >>> listToMaybe
        >>> maybe "impossible" format
        >>> putStrLn
    )

data Hit = Single Int | Double Int | Triple Int | Singlebull | Bullseye

instance Show Hit where
    show (Single n) = "single " <> show n
    show (Double n) = "double " <> show n
    show (Triple n) = "triple " <> show n
    show Singlebull = "single bull"
    show Bullseye   = "bullseye"

format :: [Hit] -> String
format = reverse
        >>> map show
        >>> unlines
        >>> init

score :: Hit -> Int
score (Single n) = n
score (Double n) = 2 * n
score (Triple n) = 3 * n
score Singlebull = 25
score Bullseye   = 50

hits :: [Hit]
hits = Singlebull : Bullseye : map Single [1..20] <> map Double [1..20] <> map Triple [1..20]

play :: Int -> [Hit] -> Int -> [[Hit]]
play _ xs 0 = case xs of
        (Double _ : _) -> [xs]
        (Bullseye : _) -> [xs]
        _              -> []
play 0 _ _ = []
play 1 xs n = map Double [1..20]
        & (Bullseye :)
        & filter (score >>> (==n))
        & map (:xs)
play t xs n = hits
        & filter (score >>> (<=n))
        & concatMap (\x -> play (t-1) (x:xs) (n - score x))
