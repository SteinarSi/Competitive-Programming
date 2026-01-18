import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:s:xs <- C.getContents <&> (C.words >>> map readInt)
    let grid = listArray ((1,1),(r,c)) xs :: UArray (Int,Int) Int
    range ((2,2),(r-1,c-1))
        & filter (\(y,x) -> grid!(y,x) == s && sum (map (grid!) [(y-1,x-1),(y-1,x+1),(y+1,x-1),(y+1,x+1)]) `mod` s == 0)
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
