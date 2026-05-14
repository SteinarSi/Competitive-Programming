import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (findIndex)
import           Data.Ratio    ((%))

main :: IO ()
main = do
    [n,m,p] <- getContents <&> (words >>> map read)
    [3,4,5,6,7,8,10,12,14,16,20,24,30]
        & dropWhile (<n)
        & findIndex (chance m >>> (>= p%100))
        & maybe "Vonlaust!" show
        & putStrLn

chance :: Integer -> Integer -> Rational
chance m n = fromIntegral (length [() | a <- [1..n], b <- [1..m], a > b]) % (n*m)
