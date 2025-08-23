import           Control.Arrow         ((***), (>>>))
import           Data.Array.Unboxed    (UArray, assocs, elems, listArray, range,
                                        (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    whd:rest <- C.getContents <&> C.lines
    let [w,h,d] = map readInt (C.words whd)
        (front,(right,top)) = concatMap (C.unpack >>> map (=='#')) rest
            & splitAt (h*w)
            & listArray ((1,1),(h,w))
                ***
              (splitAt (h*d) >>> listArray ((1,1),(h,d)) *** listArray ((1,1),(d,w)))

    putStrLn (estimate (w,h,d) front right top)

estimate :: (Int,Int,Int) -> UArray (Int,Int) Bool -> UArray (Int,Int) Bool -> UArray (Int,Int) Bool -> String
estimate (w,h,d) front right top
    | frontValid && rightValid && topValid = show (length (filter id (elems est)))
    | otherwise = "invalid"
  where
    est = listArray rng (map f (range rng)) :: UArray (Int,Int,Int) Bool
    rng = ((1,1,1),(w,h,d))
    f (x,y,z) = front ! (h-y+1,x) && right ! (h-y+1,z) && top ! (d-z+1,x)

    frontValid :: Bool
    frontValid = all (\((y,x),b) -> b == any ((x,h-y+1,) >>> (est!)) [1..d]) (assocs front)

    rightValid :: Bool
    rightValid = all (\((y,z),b) -> b == any ((,h-y+1,z) >>> (est!)) [1..w]) (assocs right)

    topValid :: Bool
    topValid = all (\((z,x),b) -> b == any ((x,,d-z+1) >>> (est!)) [1..h]) (assocs top)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
