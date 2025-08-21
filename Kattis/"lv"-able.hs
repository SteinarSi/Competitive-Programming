{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getContents

    print $ if
        | C.isInfixOf "lv" xs            -> 0
        | C.elem 'l' xs || C.elem 'v' xs -> 1
        | otherwise                      -> 2
