{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    xs <- C.getLine
    if | C.null xs        -> pure ()
       | C.last xs == '.' -> C.putStrLn "*Nod*" >> hFlush stdout >> main
       | C.last xs == '?' -> C.putStrLn "Quack!" >> hFlush stdout >> main
       | otherwise        -> pure ()
