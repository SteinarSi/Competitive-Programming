import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, listArray, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [r, c, n] <- C.getLine <&> (C.words >>> map readInt)
    weak <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b))))
    print $ runST $ do
        seen <- newArray ((1,1),(r,c)) False
        forM_ weak (flip (writeArray seen) True)
        solve 0 r c weak seen

solve :: Int -> Int -> Int -> [(Int,Int)] -> STUArray s (Int,Int) Bool -> ST s Int
solve a r c [] _ = pure a
solve a r c xs seen = do
    next <- concat <$> forM xs (\(x,y) -> do
        n <- [ (x+dx,y+dy) | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)] ]
            & filter (inRange ((1,1),(r,c)))
            & filterM (readArray seen >>> fmap not)
        forM_ n (flip (writeArray seen) True)
        pure n)
    solve (a+1) r c next seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
