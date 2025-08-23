import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([k,r]:ingr:xss) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    xss
        & map (\xs -> zipWith safeDiv ingr xs & minimum & (*last xs))
        & maximum
        & print

safeDiv :: Int -> Int -> Int
safeDiv _ 0 = maxBound
safeDiv a b = a `div` b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
