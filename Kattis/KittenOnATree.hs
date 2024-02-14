import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array.ST         (STUArray, newArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    k <- C.getLine <&> readInt
    branches <- C.getContents <&> (C.lines >>> init >>> map (C.words >>> map readInt))

    let graph = runSTUArray $ do
            arr <- newArray (1, 100) 0
            forM_ branches $ \(a:xs) -> forM_ xs (flip (writeArray arr) a)
            pure arr

    path k graph
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

path :: Int -> UArray Int Int -> [Int]
path 0 _     = []
path k graph = k : path (graph ! k) graph

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
