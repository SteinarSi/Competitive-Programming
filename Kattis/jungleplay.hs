import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, ord)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [nkm,xs] <- C.getContents <&> C.lines
    let [n,k,_] = map readInt (C.words nkm)
    print $ if n <= 1
        then 1
        else runST $ do
            seen <- newArray ('a',chr (ord 'a' + n - 1)) False
            solve n xs seen k 0 0

solve :: Int -> C.ByteString -> STUArray s Char Bool -> Int -> Int -> Int -> ST s Int
solve n xs seen k s i
    | i >= C.length xs = pure (-1)
    | otherwise        = do
        let x = C.index xs i
        r <- readArray seen x
        case r of
            True -> solve n xs seen k s (i+1)
            False | s+1 < n   -> writeArray seen x True >> solve n xs seen k (s+1) (i+1)
                  | k <= 0    -> pure (i+1)
                  | otherwise -> do
                    mapM_ (flip (writeArray seen) False) (take n ['a'..])
                    writeArray seen x True
                    solve n xs seen (k-1) 1 (i+1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
