import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (scanl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let
        search :: STUArray s Int Int -> [(Int,Int)] -> ST s String
        search seen [] = pure "-1"
        search seen ((j,x):jxs) = do
            i <- readArray seen x
            if i == -1
                then writeArray seen x j >> search seen jxs
                else pure (show (j-i) <> "\n" <> unwords (map show [i+1..j]))

    putStrLn $ runST $ do
        seen <- newArray (0,n-1) (-1)
        search seen (zip [0..] (scanl' ((+) >>> (>>> (`mod` n))) 0 xs))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
