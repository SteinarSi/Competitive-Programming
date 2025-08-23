import qualified Data.ByteString.Char8 as C
import Data.Functor ((<&>))
import Control.Arrow ((>>>), (&&&))
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.List (sort)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let k = n `div` 3
        ys = sort xs
    take k (drop k ys) <> take k ys <> drop (2*k) ys
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
