import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:p:xs <- C.getContents <&> (C.words >>> map readInt)

    print $ (solulu (sum xs) xs + p - 1) `div` p

solulu :: Int -> [Int] -> Int
solulu total []     = 0
solulu total (x:xs) = x * (total - x) + solulu (total - x) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
