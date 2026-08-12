import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust, mapMaybe)
import qualified Data.Set              as S

main :: IO ()
main = do
    [n,m,s]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            forM_ xs $ \[u,v,t,p,d] -> readArray ret v >>= (((u,t,p,d):) >>> writeArray ret v)
            pure ret

        ans = runST $ do
            dist <- newArray (0,n-1) minBound
            writeArray dist (n-1) s
            search graph dist (S.singleton (s,n-1))

    putStrLn (maybe "impossible" show ans)

search :: Array Int [(Int, Int, Int, Int)] -> STUArray s Int Int -> S.Set (Int,Int) -> ST s (Maybe Int)
search graph dist queue = case S.maxView queue of
    Nothing         -> pure Nothing
    Just ((t,v),queue')
        | v == 0    -> pure (Just t)
        | otherwise -> graph ! v
            & mapMaybe (\(u,t0,p,d) ->
                let t' = (t-d) - (t-t0-d) `mod` p
                in  if t' >= t0 then Just (t',u) else Nothing
            )
            & filter (fst >>> (>=0))
            & filterM (\(t',u) -> readArray dist u >>= \t'' -> if t'' < t' then writeArray dist u t' >> pure True else pure False)
            >>= (foldr S.insert queue' >>> search graph dist)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
