import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr)
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.Word             (Word8)

main :: IO ()
main = do
    program <- C.getContents

    let gotos = findGotos program [] M.empty 0

    runST (newArray (-80000, 80000) 0 >>= brain program gotos)
        & map (fromIntegral >>> chr)
        & putStrLn

brain :: forall s. C.ByteString -> M.IntMap Int -> STUArray s Int Word8 -> ST s [Word8]
brain program gotos memory = fuck [] 0 0
    where
        fuck :: [Word8] -> Int -> Int -> ST s [Word8]
        fuck ret p i
            | i >= C.length program = pure (reverse ret)
            | otherwise = case C.index program i of
                '>' -> fuck ret (p+1) (i+1)
                '<' -> fuck ret (p-1) (i+1)
                '+' -> do
                    r <- readArray memory p
                    maxBound == r
                        & bool (r+1) minBound
                        & writeArray memory p
                    fuck ret p (i+1)
                '-' -> do
                    r <- readArray memory p
                    minBound == r
                        & bool (r-1) maxBound
                        & writeArray memory p
                    fuck ret p (i+1)
                '.' -> readArray memory p >>= \r -> fuck (r:ret) p (i+1)
                '[' -> do
                    r <- readArray memory p
                    if r == 0
                        then fuck ret p (gotos M.! i)
                        else fuck ret p (i+1)
                ']' -> do
                    r <- readArray memory p
                    if r > 0
                        then fuck ret p (gotos M.! i)
                        else fuck ret p (i+1)
                _ -> fuck ret p (i+1)

findGotos :: C.ByteString -> [Int] -> M.IntMap Int -> Int -> M.IntMap Int
findGotos program open ret i
        | i >= C.length program = ret
        | otherwise = case (open, program `C.index` i) of
                           (_,'[')       -> findGotos program (i:open) ret (i+1)
                           (j:open',']') -> findGotos program open' (M.insert j (i+1) (M.insert i (j+1) ret)) (i+1)
                           ([],']')      -> error "bruh"
                           _             -> findGotos program open ret (i+1)
