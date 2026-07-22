import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,c]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let
        cost :: UArray Int Int
        cost = listArray (1,n) (map head xs)

        cals :: UArray Int Int
        cals = listArray (1,n) (map last xs)

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (range rng))
          where
            rng = ((0,0),(n,c))

            f :: (Int,Int) -> Int
            f (0,_) = 0
            f (i,m)
                | m < cost ! i = dp ! (i-1, m)
                | otherwise = max (dp ! (i-1, m)) (cals ! i + dp ! (i-1, m - cost ! i))

    print (dp ! (n,c))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
