import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:a:xs <- C.getContents <&> (C.words >>> map readInt)

    putStrLn $ if and (zipWith (\x y -> y-x <= a) xs (drop 1 xs))
        then "POSSIBLE"
        else "BUG REPORT"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
