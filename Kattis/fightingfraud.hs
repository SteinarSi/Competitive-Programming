{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S


main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> solve S.empty S.empty
        >>> bool "no" "yes"
        >>> C.putStrLn
    )

solve :: S.Set C.ByteString -> S.Set C.ByteString -> [[C.ByteString]] -> Bool
solve picked dropped []                 = S.size picked == S.size dropped
solve picked dropped (["pickup",x]:xs)  = S.notMember x picked && solve (S.insert x picked) dropped xs
solve picked dropped (["dropoff",x]:xs) = S.member x picked && S.notMember x dropped && solve picked (S.insert x dropped) xs
