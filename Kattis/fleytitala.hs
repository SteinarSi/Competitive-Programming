import           Data.Function ((&))

main :: IO ()
main = do
    d:k:_ <- fmap (map read . words) getContents

    map (\i -> 1 / (2^i)) [0..]
        & takeWhile (>=0.0000001)
        & take (k+1)
        & sum
        & (fromIntegral d *)
        & print
