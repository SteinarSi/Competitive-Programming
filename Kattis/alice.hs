import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn $ if and (zipWith (==) (map (`mod`k) xs) (map (`mod`k) (sort xs)))
        then "YES"
        else "NO"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
