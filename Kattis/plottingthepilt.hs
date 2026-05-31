import           Control.Arrow         (second, (>>>))
import           Data.Array.Unboxed    (UArray, assocs, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:d:xs <- C.getContents <&> (C.words >>> map readInt)

    let rng = ((1,1),(r,c))
        arr :: UArray (Int,Int) Int
        arr = listArray ((1,1),(r,c)) (map (`div` d) xs)

    assocs arr
        & map (\((i,j),x) -> [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
            & filter (inRange rng)
            & map (arr!)
            & minimum
            & format x
        )
        & chunksOf c
        & unlines
        & putStr

format :: Int -> Int -> Char
format x m = case x-m of
    1 -> '+'
    2 -> 'X'
    d | d <= 0    -> ' '
      | otherwise -> '#'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
