import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (scanl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:xs <- C.getContents <&> (C.words >>> map readInt)
    let offsets = scanl' (+) 0 xs
        mi = minimum offsets
        ma = maximum offsets
    putStrLn $ if ma-mi+1 > n
        then "finns ingen"
        else show (1 + abs mi)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
