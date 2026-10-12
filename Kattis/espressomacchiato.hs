{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.words
        >>> drop 1
        >>> concatMap (C.readInt >>> fromJust >>> fst >>> \n ->
            ".C" <> C.replicate (n-2) '.'
            : "C." <> C.replicate (n-2) '.'
            : replicate (n-2) (C.replicate n '.'))
        >>> C.unlines
    )
