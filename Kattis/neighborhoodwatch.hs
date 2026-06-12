import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:_:ks <- C.getContents <&> (C.words >>> map readInt)

    let
        walk :: Int -> Int -> [Int] -> Int
        walk !r _ []     = r
        walk !r p (x:xs) = walk (r+(x-p) * (n-x+1)) x xs

    print (walk 0 0 ks)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
