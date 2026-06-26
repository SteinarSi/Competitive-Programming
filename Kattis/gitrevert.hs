{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getLine
    putStrLn $ if revert xs
        then "revert"
        else "unrevert"

revert :: C.ByteString -> Bool
revert xs
    | C.null xs = False
    | "Revert \"" `C.isPrefixOf` xs && C.last xs == '"' = not (revert (C.init (C.drop (C.length "Revert \"") xs)))
    | otherwise = False
