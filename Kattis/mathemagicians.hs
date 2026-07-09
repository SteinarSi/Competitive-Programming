import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n',xs,ys] <- C.getContents <&> C.lines
    let n = readInt n'
        f zs = length (filter id (zipWith (/=) (C.unpack zs) (drop 1 (C.unpack zs) ++ [C.head zs])))
        xr = f xs
        yr = f ys
    putStrLn $ if yr < xr || (yr == xr && 0 < xr && xr < n)
        then "yes"
        else "no"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
