{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n <- C.getLine <&> (C.readInt >>> fromJust >>> fst)
    replicateM_ n $ do
        sounds <- C.getLine <&> C.words
        noises <- listen
        sounds
            & filter (`S.notMember` noises)
            & C.unwords
            & C.putStrLn
        pure ()

listen :: IO (S.Set C.ByteString)
listen = do
    x <- C.getLine
    if x == "what does the fox say?"
        then pure S.empty
        else fmap (S.insert (last (C.words x))) listen
