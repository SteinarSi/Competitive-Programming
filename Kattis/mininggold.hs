import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST)
import           Data.Array.ST         (STUArray, newArray, newArray_,
                                        readArray, runSTArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (Array, UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':xs <- C.getContents <&> C.lines
    let
        n = readInt n'
        (edges,queries) = splitAt (n-1) xs
            & map (C.words >>> map readInt) *** map readInt

        graph :: Array Int [(Int,Int)]
        graph = runSTArray $ do
            ret <- newArray (1,n) []
            forM_ edges $ \[a,b,c] -> do
                readArray ret a >>= (((b,c):) >>> writeArray ret a)
                readArray ret b >>= (((a,c):) >>> writeArray ret b)
            pure ret

        ans :: UArray Int Int
        ans = runSTUArray $ do
            dp <- newArray_ (1,n)
            dfs dp 0 1
            pure dp
          where
            dfs :: STUArray s Int Int -> Int -> Int -> ST s ()
            dfs dp p u = do
                let vs = filter (fst >>> (/=p)) (graph ! u)
                if null vs
                    then writeArray dp u 0
                    else do
                        mapM_ (fst >>> dfs dp u) vs
                        mapM (\(v,c) -> readArray dp v <&> (min 0 >>> (c+))) vs
                            >>= (minimum >>> writeArray dp u)

    map ((ans!) >>> show) queries
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
