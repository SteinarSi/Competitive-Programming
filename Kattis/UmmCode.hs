{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (>>>))
import           Data.Bits             (shiftL)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, isAlpha, isDigit, isSpace)
import           Data.Function         ((&))

main :: IO ()
main = C.interact $
            C.words
        >>> filter isUmm
        >>> concatMap decode
        >>> chunksOf 7
        >>> map toChar
        >>> C.pack

isUmm :: C.ByteString -> Bool
isUmm = C.all (\c -> not (isDigit c) && (not (isAlpha c) || c `C.elem` "um"))

decode :: C.ByteString -> [Int]
decode = C.unpack >>> filter isAlpha >>> map ((=='u') >>> bool 0 1)

toChar :: [Int] -> Char
toChar = reverse >>> zipWith (*) twos >>> sum >>> chr

twos :: [Int]
twos = map (shiftL 1) [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
