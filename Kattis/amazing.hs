{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (unless)
import           Data.Array.IO         (IOUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    seen <- newArray ((-100,-100),(100,100)) False
    ret  <- search seen (0,0)
    unless ret (putStrLn "no way out")

search :: IOUArray (Int,Int) Bool -> (Int,Int) -> IO Bool
search seen u@(y,x) = do
    writeArray seen u True
    anyM explore [
        ("left",(y,x-1),"right"),
        ("right",(y,x+1),"left"),
        ("down",(y+1,x),"up"),
        ("up",(y-1,x),"down")
        ]
  where
    explore :: (C.ByteString, (Int,Int), C.ByteString) -> IO Bool
    explore (q,v,b) = do
        s <- readArray seen v
        if s
            then pure False
            else do
                r <- input q
                case r of
                    "wall"   -> pure False
                    "solved" -> pure True
                    "wrong"  -> error "bruh"
                    _        -> do
                        ret <- search seen v
                        unless ret $ do
                            p <- input b
                            unless (p == "ok") $ error "bruh"
                        pure ret

input :: C.ByteString -> IO C.ByteString
input xs = C.putStrLn xs >> hFlush stdout >> C.getLine

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = pure False
anyM p (x:xs) = p x >>= bool (anyM p xs) (pure True)
