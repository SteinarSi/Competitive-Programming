{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> foldl' (\xs -> \case
            (0:x:_) -> (x+xs)
            (1:x:_) -> max 0 (xs-x)
            ) 0
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
