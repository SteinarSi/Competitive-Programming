import           Control.Arrow (first, second, (>>>))
import           Data.Array    (listArray, (!))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    ([a,b,m],words) <- getContents <&> (lines >>> splitAt 3 >>> first (map read))

    let types = map (listArray (0,m-1)) (chunksOf m words)

    iterate (\n -> (a*n+b) `mod` m) 1
        & drop 1
        & chunksOf 4
        & take m
        & map (zipWith (!) types >>> unwords >>> (<>"."))
        & unlines
        & putStr

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
