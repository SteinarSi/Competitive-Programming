import           Control.Arrow         ((***), (>>>))
import           Data.Array            (Array)
import           Data.Array.Base       (UArray, listArray, (!))
import           Data.Bits             (Bits (shiftL, xor), (.&.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Ix               (range)
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> split
        >>> map (uncurry uxuhul >>> format)
        >>> unlines
        >>> putStr
    )

uxuhul :: Int -> UArray (Int,Int) Int -> Int
uxuhul n prefs = dp ! (0,0)
  where
    rng = ((0,0),(n,7))

    dp :: Array (Int,Int) Int
    dp = listArray rng $ map f $ range rng

    f :: (Int,Int) -> Int
    f (p,b) | p == n    = b
            | otherwise = map (shiftL 1 >>> xor b >>> (p+1,) >>> (dp!)) [0..2]
                & minimumBy (compare `on` (p,) >>> (prefs!))

split :: [[Int]] -> [(Int, UArray (Int,Int) Int)]
split [] = []
split ([n]:xs) = splitAt n xs
    & (concat >>> listArray ((0,0),(n-1,7)) >>> (n,)) *** split
    & uncurry (:)

format :: Int -> String
format b = map (shiftL 1 >>> (.&. b) >>> (/=0) >>> bool 'N' 'Y') [2,1,0]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
