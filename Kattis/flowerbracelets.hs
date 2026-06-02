{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (bracelet >>> bool "counterfeit" "authentic")
        >>> C.unlines
        >>> C.putStr
    )

bracelet :: C.ByteString -> Bool
bracelet xs = "b" `C.isPrefixOf` xs && "cc" `C.isSuffixOf` xs && flowers (C.dropEnd 2 (C.drop 1 xs))

flowers :: C.ByteString -> Bool
flowers xs = daisies xs || sunflowers xs || roses xs

daisies :: C.ByteString -> Bool
daisies xs = "d" `C.isPrefixOf` xs && "d" `C.isSuffixOf` xs && flowers (C.dropEnd 1 (C.drop 1 xs))

sunflowers :: C.ByteString -> Bool
sunflowers xs = "ss" `C.isPrefixOf` xs && flowers (C.drop 2 xs)

roses :: C.ByteString -> Bool
roses xs = xs == "h" || "r" `C.isPrefixOf` xs && flowers (C.drop 1 xs)
