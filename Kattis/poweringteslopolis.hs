import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, inRange, indices, listArray,
                                        (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:xs <- C.getContents <&> C.lines

    let rng = ((0,0),(readInt n-1, readInt m-1))

        grid :: UArray (Int,Int) Char
        grid = listArray rng (concatMap C.unpack xs)

        unpowered = filter (\(y,x) -> null [() | i <- [y-1..y+1], j <- [x-1..x+1], inRange rng (i,j), grid ! (i,j) == 'T']) (indices grid)

    unpowered
        & map (\(y,x) -> show y <> " " <> show x)
        & (show (null unpowered) :)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
