{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    name <- C.getLine
    age <- C.getLine <&> readInt

    replicateM_ age (C.putStrLn ("Hipp hipp hurra, " <> name <> "!"))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
