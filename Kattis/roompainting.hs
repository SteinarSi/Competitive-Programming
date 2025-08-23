import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.lines >>> map readInt)
    let (cans, colors) = splitAt n xs
            & first S.fromList

    colors
        & map (\x -> S.lookupGE x cans
                & fromJust
                & subtract x)
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
