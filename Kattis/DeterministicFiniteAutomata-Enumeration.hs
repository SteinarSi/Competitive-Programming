import           Control.Arrow         (first, (>>>))
import           Control.Monad         (forM_)
import           Data.Array.ST         (STUArray, newArray, newArray_, runSTUArray, writeArray)
import           Data.Array.Base       (MArray, UArray, assocs, listArray, readArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ncsf:ss:fs:ls <- C.getContents <&> C.lines
    let [n,c,s,f] = C.words ncsf
            & map readInt
        sigma = C.unpack ss
        (tss,_:queries) = splitAt n ls
            & first (map (C.words >>> map readInt >>> zip sigma))
        table = runSTUArray $ do
            arr <- newArray_ ((1,'a'),(n,'z'))
            sequence_ [writeArray arr (i,c) j | (i,ts) <- zip [1..] tss, (c,j) <- ts]
            pure arr
        accepting = runSTUArray $ do
            accept <- newArray (1,n) False
            C.words fs
                & map readInt
                & mapM_ (flip (writeArray accept) True)
            pure accept

        enumeration :: UArray Int Int
        enumeration = enumerate n s table accepting ss
                & listArray (0,500000 `div` n)

    queries
        & map (readInt 
            >>> (enumeration!) 
            >>> show 
            >>> C.pack)
        & C.unlines
        & C.putStr

enumerate :: Int -> Int -> UArray (Int,Char) Int -> UArray Int Bool -> C.ByteString -> [Int]
enumerate n s table accepting sigma = simulate [(s,1)]
            & map (filter (fst >>> (accepting!)) >>> map snd >>> modSum)
    where
        simulate :: [(Int,Int)] -> [[(Int,Int)]]
        simulate xss = xss
                & map (\(state,c) -> (c, C.foldr ((state,) >>> (table!) >>> (:)) [] sigma))
                & count
                & simulate
                & (xss:)

        count :: [(Int,[Int])] -> [(Int,Int)]
        count xss = runSTUArray (do
                    arr <- newArray (1,n) 0
                    forM_ xss $ \(c,xs) -> forM_ xs (flip (modifyArray arr) ((c+) >>> modulo))
                    pure arr)
                & assocs
                & filter (snd >>> (/=0))

modSum :: [Int] -> Int
modSum = foldr ((+) >>> (>>>modulo)) 0

modulo :: Int -> Int
modulo = (`mod` 1000000007)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
