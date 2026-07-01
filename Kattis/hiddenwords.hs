import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, assocs, newArray, readArray,
                                        writeArray)
import           Data.Array.ST         (runSTArray)
import           Data.Array.Unboxed    (Array, UArray, inRange, listArray, (!))
import           Data.Bits             (popCount, setBit, testBit)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (inits)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, isJust, mapMaybe)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    hw:rest <- C.getContents <&> C.lines

    let [h,w] = map readInt (C.words hw)
        rng = ((1,1),(h,w))

        starts :: Array Char [(Int,Int)]
        starts = runSTArray $ do
            ret <- newArray ('A','Z') []
            mapM_ (\(ix,c) -> readArray ret c >>= ((ix:) >>> writeArray ret c)) (assocs grid)
            pure ret

        grid :: UArray (Int,Int) Char
        grid = listArray rng (concatMap C.unpack (take h rest))

        possible :: STRef s (M.Map C.ByteString [(Integer,(Int,Int))]) -> C.ByteString -> ST s Bool
        possible memo ys = do
            mem <- readSTRef memo
            let start = C.inits ys
                    & drop 1
                    & map (`M.lookup` mem)
                    & takeWhile isJust
                    & last
                    & fromJust
            if null start
                then pure False
                else do
                    let d = 1 + popCount (fst (head start))
                        rets = search (drop d (C.unpack ys)) start
                    modifySTRef memo (\m -> foldr (uncurry M.insert) m (drop 1 (zip (drop d (C.inits ys)) rets)))
                    pure (not (null (last rets)))
          where
            search :: String -> [(Integer, (Int,Int))] -> [[(Integer, (Int,Int))]]
            search _ [] = [[]]
            search [] xs = [xs]
            search (c:cs) xs = xs
                & concatMap (\(seen,(y,x)) -> [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
                    & filter (\v -> inRange rng v && not (testBit seen (hash v)) && grid ! v == c)
                    & map (setBit seen (hash (y,x)),))
                & search cs
                & (xs:)

    print $ runST $ do
        memo <- assocs starts
            & map (\(c,vs) -> (C.singleton c, map (0,) vs))
            & M.fromList
            & newSTRef
        drop (h+1) rest
            & filterM (possible memo)
            <&> length

hash :: (Int,Int) -> Int
hash (y,x) = 10*y + x

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
