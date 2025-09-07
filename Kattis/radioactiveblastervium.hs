import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:t:xs <- C.getContents <&> (C.words >>> map readInt)
    print (burros t xs)

burros :: Int -> [Int] -> Int
burros t [] = 0
burros t (x:xs) = t `div` x - burros t (filter (<= t) (map (lcm x) xs)) + burros t xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
