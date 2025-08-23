import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM, unless)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray, UArray, assocs, bounds, getElems,
                                   listArray, newArray, readArray, writeArray,
                                   (!))
import           Data.Function    ((&))
import           Data.Functor     ((<&>))
import           Data.Ix          (inRange)
import           Data.List        (find)

main :: IO ()
main = do
    r:c:rest <- getContents <&> words

    let grid = listArray ((1,1),(read r, read c)) (concat rest) :: UArray (Int,Int) Char
        Just (s,_) = find (snd >>> (=='S')) (assocs grid)
        ret = runST $ do
            seen <- newArray (bounds grid) False
            search grid seen s
            getElems seen <&> (filter id >>> length)

    print ret

search :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> (Int,Int) -> ST s ()
search grid seen (y,x) = do
    writeArray seen (y,x) True
    [(y-1,x),(y+1,x),(y,x+1),(y,x-1)]
        & filter (inRange (bounds grid))
        & filter ((grid!) >>> (=='#'))
        & mapM_ (\v -> readArray seen v >>= flip unless (search grid seen v))
