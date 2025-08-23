import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    cap:_:xs <- fmap (C.words >>> map readInt) C.getContents
    print (cinema cap xs)

cinema :: Int -> [Int] -> Int
cinema _ [] = 0
cinema cap (x:xs) | cap - x < 0 = 1 + cinema cap xs
                  | otherwise   = cinema (cap-x) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
