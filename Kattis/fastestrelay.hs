import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        grid :: UArray (Int,Int) Int
        grid = listArray ((1,1),(n,4)) (concat xs)

        [back,breast,butter,free] = map (\k -> take 4 (sortOn ((,k) >>> (grid!)) [1..n])) [1..4]

    print $ minimum $ do
        x1 <- back
        x2 <- filter (x1/=) breast
        x3 <- filter (x2/=) (filter (x1/=) butter)
        x4 <- filter (x3/=) (filter (x2/=) (filter (x1/=) free))

        pure (grid ! (x1,1) + grid ! (x2,2) + grid ! (x3,3) + grid ! (x4,4))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
