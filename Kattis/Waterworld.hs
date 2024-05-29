import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n:m:xs) <- C.getContents <&> (C.words >>> map readInt)
    print (fromIntegral (sum xs) / fromIntegral (n*m))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
