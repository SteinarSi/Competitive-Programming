import           Control.Arrow         ((&&&), (>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Ratio            (Ratio, denominator, numerator, (%))
import           Text.Printf           (printf)

main :: IO ()
main = do
    [n,q]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (systems,queries) = splitAt n rest

        as :: UArray Int Int
        as = listArray (1,n) (map head systems)

        bs :: UArray Int Int
        bs = listArray (1,n) (map last systems)

        convert :: Int -> Int -> Int -> String
        convert i j x = ((bj-aj) * (x-ai) % (bi-ai) + aj % 1)
                & numerator &&& denominator
                & uncurry (printf "%d/%d")
          where
            ai = as ! i
            bi = bs ! i
            aj = as ! j
            bj = bs ! j

    queries
        & map (\[i,j,x] -> convert i j x)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
