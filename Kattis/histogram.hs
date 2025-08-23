import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (transpose)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:s:_:xs <- C.getContents <&> (C.words >>> map readInt)

    let counts = runST $ do
            cnt <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            forM_ xs $ \x -> do
                let x' = (x+s-1) `div` s
                c <- readArray cnt x'
                writeArray cnt x' (c+1)
            getElems cnt
        h = maximum counts

    counts
        & map (\c -> replicate (h-c) '.' <> replicate c '#' <> "-")
        & transpose
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
