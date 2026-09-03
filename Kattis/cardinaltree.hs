import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, assocs, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Function         ((&))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nm:xs <- C.lines <$> C.getContents

    let [n,m] = map readInt (C.words nm)

        grid :: UArray (Int,Int) Char
        grid = listArray ((1,1),(n,m)) (concatMap (C.unpack >>> filter isDigit) xs)

    find (\(y,x) -> all (\((dy,dx),l) -> l == grid ! (y+dy-1,x+dx-1)) (assocs logo)) (range ((1,1),(n-4,m-2)))
        & maybe "-1" (\(i,j) -> show i <> " " <> show j)
        & putStrLn

logo :: UArray (Int,Int) Char
logo = listArray ((1,1),(5,3)) "426211526112523"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
