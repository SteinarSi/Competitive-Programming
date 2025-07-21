import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:m:xs <- C.getContents <&> (C.words >>> map readInt)

    let factorials p1 n = p1 : factorials ((n*p1) `mod` m) (n+1)
        selfinverses p2 p1 n = p2 : selfinverses p1 ((p1 + (n-1)*p2) `mod` m) (n+1)

        precomp :: UArray Int Int
        precomp = listArray (0,10^6) (zipWith (\f i -> (f-i) `mod` m) (factorials 1 1) (selfinverses 1 1 2))

    map ((precomp!) >>> show) xs
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
