import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let best | all (==1) xs = -1
             | otherwise    = kadane 0 0 (map ((==1) >>> bool 1 (-1)) xs)
    print (length (filter (1==) xs) + best)

-- This finds the best empty sequence overall, but the problem requires the best 'non-empty' sequence.
-- That's why we have to check for the all 1's edgecase.
kadane :: Int -> Int -> [Int] -> Int
kadane best curr []     = best
kadane best curr (x:xs) = kadane (max best next) next xs
  where next = max 0 (curr + x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
