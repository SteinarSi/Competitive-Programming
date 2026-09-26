import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        costs :: UArray (Int,Int) Int
        costs = listArray ((1,0),(n,m)) (concatMap (\x -> take (m+1) (x <> repeat 0)) xs)

        dp :: Array Int Int
        dp = listArray (1,n+1) (map f [1..n+1])
          where
            f :: Int -> Int
            f i
                | i == n+1  = 0
                | otherwise = [1..min m (n-i+1)]
                    & map (\j -> dp ! (i+j) - costs ! (i,j))
                    & minimum
                    & ((costs ! (i,0)) +)

    print (dp ! 1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
