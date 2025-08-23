import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let ans = runST $ do
            count <- newArray (1,50) 0 :: ST s (STUArray s Int Int)
            mapM_ (\x -> readArray count x >>= (succ >>> writeArray count x)) xs
            getAssocs count <&> (filter (snd >>> (>2*n)) >>> map fst)
    ans
        & (null >>> bool ans [-1])
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
