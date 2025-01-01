import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    _:x:y:xs <- C.getContents <&> (C.words >>> map readInt)

    countingSort xs
        & map (x*)
        & illuminate y 0 0
        & print

illuminate :: Int -> Int -> Int -> [Int] -> Int
illuminate y ret total [] = ret
illuminate y ret total (light:lights) 
    | (total+light) > y * (ret+1) = ret
    | otherwise                   = illuminate y (ret+1) (total+light) lights

countingSort :: [Int] -> [Int]
countingSort xs = runST $ do
    count <- newArray (1,20) 0 :: ST s (STUArray s Int Int)
    mapM_ (\x -> readArray count x >>= (succ >>> writeArray count x)) xs
    getAssocs count <&> concatMap (uncurry (flip replicate))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
