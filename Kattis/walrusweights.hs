import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> drop 1 >>> map readInt >>> sort >>> simplify)

    let n = length xs

        arr :: UArray Int Int
        arr = listArray (1,n) xs

        dp :: Array (Int,Int) Int
        dp = listArray ((0,0),(n,1000)) (map f (range rng))
          where
            f (0,w) = w
            f (i,w) = case compare (abs (1000-skip)) (abs (1000-pick)) of
                LT -> skip
                GT -> pick
                EQ -> max skip pick
              where
                pick
                    | w + arr ! i >= 1000 = w + arr ! i
                    | otherwise     = dp ! (i-1, w + arr ! i)
                skip = dp ! (i-1,w)
            rng = ((0,0),(n,1000))

    print (dp ! (n,0))

simplify :: [Int] -> [Int]
simplify [] = []
simplify (x:y:z:xs) | x == y && y == z = x+y : simplify (z:xs)
simplify (x:xs) = x : simplify xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
