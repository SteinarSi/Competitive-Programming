import           Control.Arrow ((&&&), (>>>))
import           Control.Monad (guard)

data Hit = Bullseye | OuterBullseye | Single Int | Double Int | Triple Int
    deriving Show

main :: IO ()
main = getLine >>= (
            read
        >>> combinations 3 []
        >>> (length >>> show) &&& concatMap format
        >>> uncurry (:)
        >>> unlines
        >>> putStr
    )

format :: [Hit] -> [String]
format hits = show (length hits) : map f hits
  where
    f OuterBullseye = "Outer bullseye"
    f x             = show x

combinations :: Int -> [Hit] -> Int -> [[Hit]]
combinations _ hits 0 = [hits]
combinations 0 _ _    = []
combinations t hits s = do
    h <- Bullseye : OuterBullseye : map Single [1..20] <> map Double [1..20] <> map Triple [1..20]
    guard (s-score h >= 0)
    combinations (t-1) (h:hits) (s-score h)

score :: Hit -> Int
score Bullseye      = 50
score OuterBullseye = 25
score (Single x)    = x
score (Double x)    = 2 * x
score (Triple x)    = 3 * x
