{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:r:c:_ <- C.getLine <&> (C.words >>> map readInt)
    C.interact (
            C.lines
        >>> splitAt r
        >>> map C.words *** chunksOf c
        >>> uncurry (zipWith (==))
        >>> map (bool "right" "left")
        >>> C.unlines
        )

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
