{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- fmap readInt C.getLine
    replicateM_ t $ do
        C.getLine
        C.getLine
        godzilla <- fmap (C.words >>> map readInt >>> maximum) C.getLine
        mechaGodzilla <- fmap (C.words >>> map readInt >>> maximum) C.getLine
        C.putStrLn $ if godzilla >= mechaGodzilla
            then "Godzilla"
            else "MechaGodzilla"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
