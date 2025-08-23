import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M

main :: IO ()
main = do
    k <- C.getLine <&> (C.words >>> last >>> C.readInt >>> fromJust >>> fst)
    xs <- C.getContents <&> (C.lines 
            >>> map (C.readInt >>> (\(Just (x,n)) -> (n,[x])))
            >>> M.fromListWith (<>)
            >>> M.assocs
            >>> zipWith (\i (_,xs) -> (i,xs)) [1..]
        )
    print $ runST $ do
        dp <- newArray ((1,0),(length xs,k)) (-1)
        arraySolve dp xs k <&> (k -)

arraySolve :: STUArray s (Int,Int) Int -> [(Int,[Int])] -> Int -> ST s Int
arraySolve dp [] r = pure r
arraySolve _  _            0 = pure 0
arraySolve dp ((n,xs):xss) r = do
    memo <- readArray dp (n,r)
    if memo >= 0
        then pure memo
        else do
            y <- filter (<=r) xs
                & (0:)
                & mapM ((r-) >>> arraySolve dp xss)
                <&> minimum
            writeArray dp (n,r) y
            pure y
