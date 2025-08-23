{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getContents
    C.putStrLn $ if "69" `C.isInfixOf` xs || "420" `C.isInfixOf` xs
        then "Mergjad!"
        else "Leim!"
