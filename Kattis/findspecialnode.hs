import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_, replicateM, unless,
                                        when, zipWithM_)
import           Data.Array            (Array, (!))
import           Data.Array.IO         (IOUArray, getAssocs)
import           Data.Array.ST         (newArray, readArray, runSTArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.IORef            (IORef, modifyIORef', newIORef,
                                        readIORef, writeIORef)
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    [n,m] <- C.getLine <&> (C.words >>> map readInt)
    xs <- replicateM m (C.getLine <&> (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (1,n) []
            zipWithM_ (\i [u,v] -> do
                readArray ret u >>= (((i,v):) >>> writeArray ret u)
                readArray ret v >>= (((i,u):) >>> writeArray ret v)
                ) [1..] xs
            pure ret

    possible  <- newArray @IOUArray (1,n) True
    current   <- newArray @IOUArray (1,n) False
    selection <- newArray @IOUArray (1,m) False
    rem       <- newIORef n

    let
        search :: Int -> IO ()
        search u = forM_ (graph ! u) $ \(i,v) -> do
            d <- readIORef rem
            unless (d <= 0) $ do
                writeArray selection i True
                p <- readArray possible v
                c <- readArray current v
                unless c $ do
                    writeArray current v True
                    when p (modifyIORef' rem pred)
                    search v

        solve :: Int -> IO ()
        solve ps
            | ps <= 1 = getAssocs possible >>= (find snd >>> fromJust >>> fst >>> show >>> ("! " <>) >>> putStrLn)
            | otherwise = do
                writeArray current 1 True
                poss1 <- readArray possible 1
                writeIORef rem (ps `div` 2)
                when poss1 (modifyIORef' rem pred)
                search 1

                sel <- getAssocs selection
                putStrLn (unwords ("?" : map (snd >>> bool "0" "1") sel))
                hFlush stdout
                poss <- getLine <&> (=="1")

                ps' <- filterM (\u -> do
                    c <- readArray current u
                    p <- readArray possible u
                    writeArray current u False
                    if p
                        then writeArray possible u (poss==c) >> pure (poss==c)
                        else pure False
                    ) [1..n] <&> length
                mapM_ (\(uv,s) -> when s (writeArray selection uv False)) sel
                solve ps'

    solve n

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
