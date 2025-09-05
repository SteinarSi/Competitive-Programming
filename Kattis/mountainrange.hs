import           Control.Arrow         (second, (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (dropWhileEnd, sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let rng = ((1,1),(100,200))

        grid = runST $ do
            ret <- newArray rng ' '
            mapM_ (mount ret) (sortOn (\[_,_,z,_,_] -> -z) xs)
            getElems ret

    grid
        & chunksOf 200
        & map (dropWhileEnd (==' '))
        & filter (null >>> not)
        & reverse
        & unlines
        & putStr

mount :: forall s. STUArray s (Int,Int) Char -> [Int] -> ST s ()
mount ret [x,h,_,t,s] = do
    draw '/'  [(i,x-h+i) | i <- [1..h]]
    draw '\\' [(i,x+1+h-i) | i <- [1..h]]
    mapM_ (row 'Y') [1..t]
    mapM_ (row ' ') [t+1..s]
    mapM_ (row '*') [s+1..h-1]
  where
    draw :: Char -> [(Int,Int)] -> ST s ()
    draw c = mapM_ (flip (writeArray ret) c)

    row :: Char -> Int -> ST s ()
    row c i = draw c [(i,j) | j <- [x-h+i+1 .. x+h-i]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
