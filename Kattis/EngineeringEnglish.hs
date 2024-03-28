{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (C.lines
        >>> map C.words
        >>> foldl (\(seen,xs) w -> fmap (:xs) (scan w seen)) (S.empty, [])
        >>> snd
        >>> reverse
        >>> mapM_ (C.unwords >>> C.putStrLn)
    )

scan :: [C.ByteString] -> S.Set C.ByteString -> (S.Set C.ByteString, [C.ByteString])
scan [] seen = (seen, [])
scan (x:xs) seen | S.member (C.map toUpper x) seen = fmap (".":) (scan xs seen)
                 | otherwise = fmap (x:) (scan xs (S.insert (C.map toUpper x) seen))
