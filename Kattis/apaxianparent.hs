{-# LANGUAGE MultiWayIf #-}

import           Data.List (isSuffixOf)

main :: IO ()
main = do
    [y,p] <- fmap words getLine
    putStrLn $ if
        | "e" `isSuffixOf` y   -> y ++ "x" ++ p
        | elem (last y) "aiou" -> init y ++ "ex" ++ p
        | "ex" `isSuffixOf` y  -> y ++ p
        | otherwise            -> y ++ "ex" ++ p
