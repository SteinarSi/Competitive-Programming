import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k,m,d] <- getContents <&> (words >>> map read)
    putStrLn (maybe "impossible" show (bruteForce (k,m,n) 0 d))

bruteForce :: (Int,Int,Int) -> Int -> Int -> Maybe Int
bruteForce (k,m,n) c d
    | d == 6                    = succ <$> bruteForce (k,m,n) (max 0 (c-m)) 7
    | d == 7                    = succ <$> bruteForce (k,m,n) (max 0 (c-m)) 1
    | c + k >= n                = Just 1
    | (5*k <= 2*m && c+5*k < n) = Nothing
    | otherwise                 = succ <$> bruteForce (k,m,n) (c + k) (d+1)
