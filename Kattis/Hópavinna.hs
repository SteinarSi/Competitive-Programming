import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    x:xs <- C.getContents <&> (C.words >>> drop 1 >>> map readInt)

    print $ sum (x:xs) - solve 0 x xs

solve :: Int -> Int -> [Int] -> Int
solve opt1 opt2 []     = opt2
solve opt1 opt2 (x:xs) = solve opt2 (max (x+opt1) opt2) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
