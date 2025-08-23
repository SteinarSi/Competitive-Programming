{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> (\(x:xs) -> shiritori (S.singleton x) x True xs)
        >>> C.putStrLn
    )

shiritori :: S.Set C.ByteString -> C.ByteString -> Bool -> [C.ByteString] -> C.ByteString
shiritori _ _ _ [] = "Fair Game"
shiritori seen prev p (x:xs) | C.head x /= C.last prev || S.member x seen = "Player " <> bool "1" "2" p <> " lost"
                             | otherwise = shiritori (S.insert x seen) x (not p) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
