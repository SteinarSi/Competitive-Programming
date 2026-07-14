import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [du,dn]:[r,c]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let
        rng = ((1,1),(r,c))
        [di,dj] = last rest

        grid :: UArray (Int,Int) Int
        grid = init rest
            & concat
            & listArray rng

        reachable :: (Int,Int) -> (Int,Int) -> Bool
        reachable s t = runST $ do
            seen <- newArray rng False
            writeArray seen s True
            dfs seen s
            readArray seen t
          where
            dfs :: STUArray s (Int,Int) Bool -> (Int,Int) -> ST s ()
            dfs seen (y,x) = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
                & filter (inRange rng)
                & filter ((grid!) >>> inRange (grid ! (y,x) - dn, grid ! (y,x) + du))
                & filterM (\v -> readArray seen v >>= \s -> if s then pure False else writeArray seen v True >> pure True)
                >>= mapM_ (dfs seen)

    putStrLn $ if reachable (1,1) (di,dj) && reachable (di,dj) (1,1)
        then "Kvoldinu er bjargad!"
        else "Nu er Eyleifur i bobba!"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
