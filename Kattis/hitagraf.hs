import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)


main :: IO ()
main = do
    _:r:xs <- C.getContents <&> (C.lines >>> map (C.words >>> last >>> readInt))
    xs
        & map (pred >>> (`div` 80) >>> ("Kk.hH"!!))
        & chunksOf r
        & unlines
        & putStr

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
