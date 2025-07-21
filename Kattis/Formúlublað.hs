import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    [n,l]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let lin = listArray (1,n) (map head xs) :: UArray Int Int
        imp = listArray (1,n) (map last xs) :: UArray Int Int

        rng = ((0,0),(n,l))

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Int
        f (0,_) = 0
        f (i,r)
            | lin ! i <= r = max (imp ! i + dp ! (i-1,r-lin!i)) (dp ! (i-1,r))
            | otherwise    = dp ! (i-1,r)

        backtrack :: (Int,Int) -> [Int]
        backtrack (0,_) = []
        backtrack (i,r) | lin ! i > r || imp ! i + dp ! (i-1,r-lin!i) <= dp ! (i-1,r) = backtrack (i-1,r)
                        | otherwise = i-1 : backtrack (i-1,r-lin!i)

    let picks = backtrack (n,l)

    printf "%d %d\n%s\n" (length picks) (dp ! (n,l)) (unwords (map show picks))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
