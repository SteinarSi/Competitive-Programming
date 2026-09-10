import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bits             (xor)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n],xs,ys] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print $ runST (newArray (0,2*n-1) False >>= \seen -> play seen (1,0,ys) Nothing (0,0,xs))

play :: STUArray s Int Bool -> (Int,Int,[Int]) -> Maybe Int -> (Int,Int,[Int]) -> ST s Int
play seen (p2,s2,_) _ (p1,s1,[]) = pure $ case compare s1 s2 of
    LT -> p2
    EQ -> -1
    GT -> p1
play seen (p2,s2,ys) picked (p1,s1,x:xs) = do
    k1 <- readArray seen x
    if k1
        then play seen (p2,s2,ys) picked (p1,s1,xs)
        else do
            let y = x `xor` 1
            writeArray seen x True
            k2 <- readArray seen y
            case (k2,picked) of
                (True,Nothing)  -> play seen (p2,s2,ys) Nothing (p1,s1+1,xs)
                (True,Just p)
                    | p == y    -> play seen (p2,s2,ys) Nothing (p1,s1+1,xs)
                    | otherwise -> play seen (p1,s1,xs) Nothing (p2,s2+1,ys)
                (False,Nothing) -> play seen (p2,s2,ys) (Just x) (p1,s1,xs)
                (False,Just _)  -> play seen (p1,s1,xs) Nothing (p2,s2,ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
