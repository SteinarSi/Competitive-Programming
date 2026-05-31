import           Control.Arrow         ((***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    lfm:rest <- C.getContents <&> (C.lines >>> map C.words)

    let [l,f,m] = map readInt lfm
        (ls,fs) = splitAt l (map (map C.head) rest)
        d = runST $ do
            lead <- count ls
            foll <- count fs
            dances (lead,foll) chances 0

    print (fromIntegral (d * m) / 3)

dances :: (STUArray s Int Int, STUArray s Int Int) -> [(Int,Int,Int)] -> Int -> ST s Int
dances _ [] r = pure r
dances (lead,foll) ((i,j,c):xs) r = do
    l <- readArray lead i
    f <- readArray foll j
    if l <= 0 || f <= 0
        then dances (lead,foll) xs r
        else do
            writeArray lead i (l-1)
            writeArray foll j (f-1)
            dances (lead,foll) ((i,j,c):xs) (r+c)

count :: [[Char]] -> ST s (STUArray s Int Int)
count xs = do
    ret <- newArray (0,7) (0::Int)
    xs
        & map (\x -> sum (map (\(d,s) -> bool 0 s (d `elem` x)) [('s',1),('c',2),('b',4)]))
        & mapM_ (\i -> readArray ret i >>= (succ >>> writeArray ret i))
    pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

chances :: [(Int,Int,Int)]
chances = concatMap (\(i,j,c) -> bool [(i,j,c),(j,i,c)] [(i,j,c)] (i==j)) [
        (scb,scb,3),
        (sc,sc,2),
        (sb,sb,2),
        (cb,cb,2),
        (scb,sc,2),
        (scb,sb,2),
        (scb,cb,2),
        (s,s,1),
        (c,c,1),
        (b,b,1),
        (scb,s,1),
        (scb,c,1),
        (scb,b,1),
        (sc,s,1),
        (sc,b,1),
        (sb,s,1),
        (sb,b,1),
        (cb,b,1),
        (cb,c,1)
    ]
  where
    s = 1
    c = 2
    sc = 3
    b = 4
    sb = 5
    cb = 6
    scb = 7
