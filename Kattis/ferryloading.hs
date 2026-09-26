import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, newArray_,
                                        unsafeRead, unsafeWrite)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    l:xs' <- C.getContents <&> (C.lines >>> init >>> map readInt)

    let xs = constrain (200*l) xs'
        n = fromIntegral (length xs)
        zipped = zip3 [n,n-1..] xs (reverse (scanl1 (+) (reverse xs)))

    putStr $ unlines $ runST $ do
        seen <- newArray (0, hash (10000,10000)) False
        lr   <- newArray_ (0, hash (10000,10000))
        r    <- forwards seen lr (100*l,100*l) zipped
        ret  <- backtrack lr (100*l,100*l) [] zipped
        pure (show r : ret)

forwards :: STUArray s Int Bool -> STUArray s Int Bool -> (Int,Int) -> [(Int,Int,Int)] -> ST s Int
forwards seen lr (l,r) [] = pure 0
forwards seen lr (l,r) ((i,x,rem):xs)
    | rem <= l || rem <= r = pure i
    | x>l && x>r           = pure 0
    | x>l                  = forwards seen lr (l,r-x) xs <&> succ
    | x>r || l==x          = forwards seen lr (l-x,r) xs <&> succ
    | otherwise = do
        let h = hash (l,r)
        s <- unsafeRead seen h
        if s
            then pure 0
            else do
                unsafeWrite seen h True
                l' <- forwards seen lr (l-x,r) xs
                r' <- forwards seen lr (l,r-x) xs
                if l' > r'
                    then do
                        unsafeWrite lr h False
                        pure (l'+1)
                    else do
                        unsafeWrite lr h True
                        pure (r'+1)

backtrack :: STUArray s Int Bool -> (Int,Int) -> [String] -> [(Int,Int,Int)] -> ST s [String]
backtrack lr (l,r) ret [] = pure (reverse ret)
backtrack lr (l,r) ret ((i,x,rem):xs)
    | rem <= l                = pure (reverse ret <> replicate i "port")
    | rem <= r                = pure (reverse ret <> replicate i "starboard")
    | x>l && x>r              = pure (reverse ret)
    | x>l                     = backtrack lr (l,r-x) ("starboard":ret) xs
    | x>r || l==x             = backtrack lr (l-x,r) ("port":ret) xs
    | otherwise               = do
        right <- unsafeRead lr (hash (l,r))
        if right
            then backtrack lr (l,r-x) ("starboard":ret) xs
            else backtrack lr (l-x,r) ("port":ret) xs

hash :: (Int,Int) -> Int
hash (l,r) = l * 10000 + r

constrain :: Int -> [Int] -> [Int]
constrain _ []     = []
constrain r (x:xs)
    | x <= r    = x : constrain (r-x) xs
    | otherwise = []

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
