import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:xs <- C.getContents <&> (C.words >>> map readInt)
    let forecast = listArray ((1,1),(n,m)) xs :: UArray (Int,Int) Int
    putStrLn $ if null (do
            y <- [2..n-1]
            x <- [2..m-1]
            guard (all (\p -> forecast ! (y,x) < forecast ! p) [(y-1,x),(y+1,x),(y,x-1),(y,x+1)])
            pure ()
        )
        then "Neibb"
        else "Jebb"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
