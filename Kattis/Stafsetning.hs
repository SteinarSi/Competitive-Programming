import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:m:k:xs <- C.getContents <&> (C.words >>> map readInt)

    let capacity = k `div` m
        days = (sum xs + capacity - 1) `div` capacity

    putStrLn $ if m > k
        then ":("
        else show days

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
