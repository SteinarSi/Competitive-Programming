{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find, transpose)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.words
        >>> drop 2
        >>> iterate rotate
        >>> take 4
        >>> filter valid
        >>> find valid
        >>> maybe "impossible\n" format
    )

format  :: [C.ByteString] -> C.ByteString
format xss = map (C.map ((=='#') >>> bool '#' '.')) xss
    & C.transpose
    & map C.reverse
    & (C.unwords (map (show >>> C.pack) [C.length (head xss), length xss]) :)
    & C.unlines

valid :: [C.ByteString] -> Bool
valid = all (\x -> C.length (C.takeWhile (=='#') x) + C.length (C.takeWhileEnd (=='.') x) == C.length x)

rotate :: [C.ByteString] -> [C.ByteString]
rotate = map C.reverse >>> C.transpose

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
