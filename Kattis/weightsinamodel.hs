import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    x1:x2:a:b:c:d:_:xs <- C.getContents <&> (C.words >>> map readInt)

    let
        f p2 p1 n
            | even n    = p2 : f p1 ((a*p1 + b*n ) `mod` 1000000007) (n+1)
            | otherwise = p2 : f p1 ((c*p1 + d*p2) `mod` 1000000007) (n+1)

        memo :: UArray Int Int
        memo = listArray (1,100000) (f x1 x2 3)

    xs
        & map ((memo!) >>> show)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
