import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import           Data.Array.Unboxed    (UArray, inRange, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nm:xs <- C.lines <$> C.getContents
    let
        [n,m] = map readInt (C.words nm)
        rng = ((1,1),(n,m))

        grid :: UArray (Int,Int) Bool
        grid = listArray rng (concatMap (C.unpack >>> map (=='#')) xs)

        search :: (Int,Int) -> (forall a. [a])
        search (h,w) = do
            y <- [1..n-h+1]
            x <- [1..m-w+1]
            guard (all (grid !) (range ((y,x),(y+h-1,x+w-1))))
            let hori  = concatMap (\x' -> [(y-1,x'),(y+h,x')]) [x-1..x+w]
                verti = concatMap (\y' -> [(y',x-1),(y',x+w)]) [y..y+h-1]
            guard (all (\ix -> not (inRange rng ix) || not (grid ! ix)) (hori <> verti))
            pure undefined

        monoliths = length $ do
            h <- [1,4,9]
            w <- [1,4,9]
            guard (h /= w)
            search (h,w)

    putStrLn $ case monoliths of
        0 -> "Ekkert fannst"
        1 -> "Steinn fannst"
        _ -> "Bilun"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
