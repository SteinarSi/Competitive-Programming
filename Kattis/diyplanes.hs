import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (Ix, MArray, STArray, STUArray, freeze,
                                        newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    putStrLn $ runST $ do
        indeg <- newArray (0,n-1) 0
        graph <- newArray (0,n-1) [] :: ST s (STArray s Int [Int])
        forM_ xs $ \(a,b) -> do
            modifyArray indeg b succ
            modifyArray graph a (b:)
        grph <- freeze graph
        src <- filterM (readArray indeg >>> (<&> (==0))) [0..n-1]
        solve grph indeg n src

solve :: Array Int [Int] -> STUArray s Int Int -> Int -> [Int] -> ST s String
solve _ _ 0 _ = pure "YES"
solve _ _ _ [] = pure "NO"
solve _ _ _ (_:_:_) = pure "NO"
solve graph indeg i [u] = graph ! u
    & filterM (flip (modifyArray indeg) pred >>> (<&> (==0)))
    >>= solve graph indeg (i-1)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m b
modifyArray arr ix f = do
    x <- readArray arr ix
    writeArray arr ix (f x)
    pure (f x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
