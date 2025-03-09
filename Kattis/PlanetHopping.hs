import           Control.Arrow         ((>>>))
import           Data.Array            (Array, inRange, range)
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:f:xs <- C.getContents <&> (C.words >>> map readInt)
    print (solve n f (listArray ((1,1),(n,n)) xs))

solve :: Int -> Int -> UArray (Int,Int) Int -> Int
solve n f matrix = maximum $ do
    b <- [1,n]
    c <- [1..n]
    [dp ! (f,(b,c)), dp ! (f,(c,b))]
  where
    rng :: ((Int,(Int,Int)), (Int,(Int,Int)))
    rng = ((0,(1,1)),(f,(n,n)))

    dp :: Array (Int,(Int,Int)) Int
    dp = listArray rng (map opt (range rng))

    opt :: (Int,(Int,Int)) -> Int
    opt (0,u) = matrix ! u
    opt (r,u@(y,x)) = [(y+1,x),(y-1,x),(y,x+1),(y,x-1)]
            & map (r-1,)
            & filter (inRange rng)
            & filter (snd >>> (matrix!) >>> (>(matrix!u)))
            & map (dp!)
            & (0:)
            & maximum
            & ((matrix!u) +)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
