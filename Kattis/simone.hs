import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, getElems, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)
    let next = runST (solve k xs)
    print (length next)
    C.putStrLn . C.unwords $ map (show >>> C.pack) next

solve :: Int -> [Int] -> ST s [Int]
solve k xs = do
    count <- newArray (1,k) 0 :: ST s (STUArray s Int Int)
    forM_ xs $ \x -> readArray count x >>= (succ >>> writeArray count x)
    least <- getElems count <&> minimum
    getAssocs count <&> (filter (snd >>> (least==)) >>> map fst)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
