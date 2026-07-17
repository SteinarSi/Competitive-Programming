import           Control.Arrow         ((***), (>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

data Cost a = Finite a | Infinite
    deriving (Eq,Ord,Functor)

main :: IO ()
main = do
    [s]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let criteria = 1 + sum (map head xs) `div` 2
        (won,undecided) = partition (\[_,c,f,u] -> c > f+u) xs
            & (map head >>> sum) *** (filter (\[_,c,f,u] -> f < c+u) >>> map (\[d,c,f,u] -> (d, (1 + (c+f+u) `div` 2) - c)))
        n = length undecided
        rng = ((1,0),(n,criteria-1))

        dp :: Array (Int,Int) (Cost Int)
        dp = listArray rng $ do
            (i,(d,r)) <- zip [1..n] undecided
            x <- [0..criteria-1]
            let pick | x+d >= criteria = Finite r
                     | i > 1 = (r+) <$> dp ! (i-1,x+d)
                     | otherwise = Infinite
                skip | i > 1 = dp ! (i-1,x)
                     | otherwise = Infinite
            pure (min pick skip)

    putStrLn $ case dp ! (n,won) of
        _ | won >= criteria -> "0"
          | n == 0          -> "impossible"
        Infinite            -> "impossible"
        Finite s            -> show s

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
