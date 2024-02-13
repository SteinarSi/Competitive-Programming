import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, liftM2, replicateM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray_, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, k] <- fmap (C.words >>> map readInt) C.getLine
    when ((n,k) /= (0,0)) $ replicateM n C.getLine >>= (
                map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b)))
            >>> solve k
            >>> show
            >>> C.pack
            >>> C.putStrLn
        )

solve :: Int -> [(Int,Int)] -> Int
solve 0 rooms = sum (map fst rooms ++ map snd rooms)
solve k ((ll,rr):rooms) = runST $ do
    let n = length rooms + 1
    optleft  <- newArray_ ((0,0),(k,n)) :: ST s (STUArray s (Int,Int) Int)
    optright <- newArray_ ((0,0),(k,n)) :: ST s (STUArray s (Int,Int) Int)

    writeArray optleft  (0,0) (ll+rr)
    writeArray optright (0,0) (ll+rr)

    forM_ (zip [1..] rooms) $ \(i,(l,r)) -> do
        v <- (l+r+) <$> liftM2 max (readArray optleft (0,i-1)) (readArray optright (0,i-1))
        writeArray optleft  (0,i) v
        writeArray optright (0,i) v

    writeArray optleft  (1,0) rr
    writeArray optright (1,0) ll

    forM_ [2..k] $ \kk -> do
        writeArray optleft  (kk,0) (-999999)
        writeArray optright (kk,0) (-999999)

    forM_ [(room, k') | room <- zip [1..] rooms, k' <- [1..k]] $ \((i,(l,r)), kk) -> do
        pl <- readArray optleft  (kk,i-1)
        pr <- readArray optright (kk,i-1)
        cl <- readArray optleft  (kk-1,i-1)
        cr <- readArray optright (kk-1,i-1)

        writeArray optleft  (kk,i) $ maximum [ pl+l+r, pr+l+r, cl+r ]
        writeArray optright (kk,i) $ maximum [ pl+l+r, pr+l+r, cr+l ]

    liftM2 max (readArray optleft (k,n-1)) (readArray optright (k,n-1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
