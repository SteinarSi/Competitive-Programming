import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM, when)
import           Control.Monad.ST      (ST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = do
    [n,x,k]:hs:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        height :: UArray Int Int
        height = listArray (1,n) hs

        graph :: UArray (Int,Int) Int
        graph = listArray ((1,1),(n,n)) (concat rest)

        dist :: UArray Int Int
        dist = runSTUArray $ do
            ret <- newArray (1,n) maxBound
            writeArray ret x 0
            bfs n graph ret [x]
            pure ret

        dp :: Array (Int,Int) (Maybe Int)
        dp = listArray rng (map f (range rng))
          where
            rng = ((1,0),(n,k))
            f (u,r)
                | u == x = Just 0
                | otherwise = case next of
                    [] -> Nothing
                    xs -> Just (minimum xs)
              where
                next = [1..n]
                    & filter (\v -> dist ! v < dist ! u && height ! v - height ! u <= r)
                    & mapMaybe (\v -> (graph ! (u,v) +) <$> dp ! (v, r - max 0 (height ! v - height ! u)))

    putStrLn (maybe "-1" show (dp ! (1, k)))

bfs :: Int -> UArray (Int,Int) Int -> STUArray s Int Int -> [Int] -> ST s ()
bfs _ _ _ [] = pure ()
bfs n graph dist xs = forM xs (\u -> do
        d <- readArray dist u
        filterM (\v -> do
            let l = d + graph ! (v,u)
            s <- readArray dist v
            when (l < s) (writeArray dist v l)
            pure (l < s)) [1..n])
        >>= (concat >>> bfs n graph dist)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
