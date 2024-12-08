{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import Data.Functor ((<&>))

main :: IO ()
main = do
    [_,x,xs] <- C.getContents <&> C.lines

    C.putStrLn $ if C.head x `C.elem` xs
        then "Unnar fann hana!"
        else "Unnar fann hana ekki!"
