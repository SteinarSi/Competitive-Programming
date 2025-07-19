import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, assocs, bounds, inRange,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n':xs <- C.getContents <&> C.lines
    let n = readInt n'
        grid = listArray ((1,1),(n,n)) (concatMap C.unpack xs) :: UArray (Int,Int) Char
        letters = filter (snd >>> isAlpha) (assocs grid)
    print (solve grid S.empty letters)

solve :: UArray (Int,Int) Char -> S.Set (Char,Char,Char) -> [((Int,Int),Char)] -> Int
solve grid triplets []               = S.size triplets
solve grid triplets (((x1,y1),a):xs) = solve grid (foldl' (flip S.insert) triplets (concatMap line xs)) xs
  where
    line :: ((Int,Int),Char) -> [(Char,Char,Char)]
    line ((x2,y2),b) = map (\i -> (x2+dx*i,y2+dy*i)) [1..]
        & takeWhile (inRange (bounds grid))
        & map (grid!)
        & filter isAlpha
        & map (a,b,)
      where
        dx' = x2-x1
        dy' = y2-y1
        dx = dx' `div` gcd dx' dy'
        dy = dy' `div` gcd dx' dy'

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
