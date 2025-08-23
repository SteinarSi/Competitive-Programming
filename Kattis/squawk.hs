import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array            (Array, Ix)
import           Data.Array.Base       (MArray, UArray, assocs, bounds,
                                        newArray, readArray, writeArray, (!))
import           Data.Array.ST         (runSTArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,s,t]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            forM_ xs $ \[x,y] -> do
                modifyArray ret x (y:)
                modifyArray ret y (x:)
            pure ret

    [(s,1)]
        & iterate (squawk graph)
        & (!! t)
        & map snd
        & sum
        & print

squawk :: Array Int [Int] -> [(Int,Int)] -> [(Int,Int)]
squawk graph xs = assocs $ runSTUArray $ do
    ret <- newArray (bounds graph) 0
    forM_ xs $ \(x,s) -> mapM_ (flip (modifyArray ret) (s+)) (graph!x)
    pure ret

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
