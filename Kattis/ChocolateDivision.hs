{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    x:y:_ <- C.getContents <&> (C.words >>> map readInt)

    let moves = (x - 1) + (y-1) * x

    C.putStrLn $ if odd moves
        then "Alf"
        else "Beata"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
