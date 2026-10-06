import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (newArray, readArray, writeArray, (!))
import           Data.Array.ST         (STUArray, runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        (edges,[s,t]:_:fs) = splitAt m xs
        graph = runSTArray $ do
            ret <- newArray (1,n) []
            mapM_ (\[u,v] -> do
                readArray ret u >>= ((v:) >>> writeArray ret u)
                readArray ret v >>= ((u:) >>> writeArray ret v)
                ) edges
            pure ret

        distance :: [Int] -> Maybe Int
        distance ss = runST $ do
            seen <- newArray (1,n) False
            mapM_ (flip (writeArray seen) True) ss
            bfs seen 0 ss

        bfs :: STUArray s Int Bool -> Int -> [Int] -> ST s (Maybe Int)
        bfs seen _ [] = pure Nothing
        bfs seen d ys
            | elem t ys = pure (Just d)
            | otherwise = concatMap (graph !) ys
                & filterM (\x -> readArray seen x >>= \s -> if s then pure False else writeArray seen x True >> pure True)
                >>= bfs seen (d+1)

    putStrLn $ case (distance [s], distance (concat fs)) of
        (Just _, Nothing)            -> "YES"
        (Just d1, Just d2) | d1 < d2 -> "YES"
        _                            -> "NO"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
