import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (Ix, MArray (..), readArray, runSTArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            arr <- newArray (1,n) []
            forM_ xs $ \[u,v] -> do
                modifyArray arr u (v:)
                modifyArray arr v (u:)
            pure arr

    print (solve n graph)

solve :: Int -> Array Int [Int] -> Double
solve n graph = sum (map centrality [1..n]) / fromIntegral n
  where
    centrality :: Int -> Double
    centrality u = degrees u u 1
        & map (fromIntegral >>> (1/))
        & sum
        & (1+)

    degrees :: Int -> Int -> Int -> [Int]
    degrees p u d = graph ! u
        & filter (/=p)
        & concatMap (\v -> let d' = d * length (graph ! v) in d' : degrees u v d')

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
