import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines >>> mapM_ (C.words >>> map readInt >>> solve >>> print))

solve :: [Int] -> Int
solve xs = fromJust (find (\x -> sum xs - x == x) xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
