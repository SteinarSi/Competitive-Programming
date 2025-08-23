import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

main :: IO ()
main = do
    C.getLine
    lo <- quack (-(10^9))
    hi <- quack (2*10^9)
    if lo == hi
        then answer [lo]
        else play (S.fromList [lo,hi]) [(lo+1,hi-1)] >>= answer

play :: S.IntSet -> [(Int,Int)] -> IO [Int]
play ducks [] = pure (S.toList ducks)
play ducks ((lo,hi):xs) | lo > hi   = play ducks xs
                        | otherwise = do
    let mi = (lo+hi+1) `div` 2
    q <- quack mi
    play (S.insert q ducks) $ case compare q mi of
        _ | q < lo -> updateBelow q xs
          | q > hi -> updateAbove q xs
        LT                   -> (lo,q-1):(2*mi-q,hi):xs
        EQ                   -> (lo,mi-1):(mi+1,hi):xs
        GT                   -> (lo,2*mi-q-1):(q+1,hi):xs

  where
    updateAbove :: Int -> [(Int,Int)] -> [(Int,Int)]
    updateAbove q [] = []
    updateAbove q ((x,y):xs)
        | q < x || q > y = (x,y) : updateAbove q xs
        | otherwise      = (q+1,y) : updateAbove q xs

    updateBelow :: Int -> [(Int,Int)] -> [(Int,Int)]
    updateBelow q [] = []
    updateBelow q ((x,y):xs)
        | q < x || q > y = (x,y) : updateBelow q xs
        | otherwise      = (x,q-1) : updateBelow q xs

answer :: [Int] -> IO ()
answer xs = printf "! %d\n%s\n" (length xs) (unwords (map show xs))

quack :: Int -> IO Int
quack = show
    >>> ("? "<>)
    >>> putStrLn
    >>> (>> (hFlush stdout >> C.getLine <&> readInt))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
