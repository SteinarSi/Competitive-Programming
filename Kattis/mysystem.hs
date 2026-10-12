{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           System.IO             (BufferMode (..), hFlush, stdout)

main :: IO ()
main = do
    t <- read <$> getLine :: IO Int
    play (100*t) generator
    pure ()

play :: Int -> [Bool] -> IO ()
play 0 _ = pure ()
play i (b:bs) = do
    putStrLn (format b)
    hFlush stdout
    y <- C.getLine
    putStrLn (format (y /= "T"))
    hFlush stdout
    C.getLine
    play (i-1) bs

format :: Bool -> String
format True  = "T"
format False = "F"

generator :: [Bool]
generator = cycle [False,True,True,True,False,True,False,True,True,True,True,False,False,True,True,False,True,False,False,False,True,True,True,False,True,True,True,False,False,True,False,False,False,False,True,True,False,True,False,False,True,False,True,False,True,False,False,True,True,True,False,False,True,True,False,False,False,True,True,True,True,False,False,True,False,True,True,False,True,True,False,False,True,False,False,False,True,True,True,False,False,True,False,False,False,False,True,True,True,False,False,True,False,True,True,True,True,False,False,True,False]
