import           Control.Arrow (second, (>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (sort, transpose)

main :: IO ()
main = do
    xss <- getContents <&> (lines >>> map (words >>> map read))

    let chunks = map (chunksOf 3) xss
        boxes = [ concatMap (!!j) (take 3 (drop (3*i) chunks)) | i <- [0..2], j <- [0..2]]
        angles = xss <> transpose xss <> boxes

    putStrLn $ if all valid angles
        then "VALID"
        else "INVALID!"

valid :: [Int] -> Bool
valid = sort >>> (==[1..9])

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
