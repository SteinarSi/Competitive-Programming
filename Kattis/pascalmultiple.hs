import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)
    rowByRow [1]
        & take (fromIntegral n + 1)
        & concatMap (filter ((`mod`k) >>> (==0)))
        & length
        & print

rowByRow :: [Integer] -> [[Integer]]
rowByRow xs = xs : rowByRow (zipWith (+) (0 : xs)  (xs ++ [0]))
