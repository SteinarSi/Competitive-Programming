{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import qualified Data.Set              as S

main :: IO ()
main = do
    names <- C.getContents <&> (
                C.lines
            >>> map (C.words >>> (\(a:b:_) -> (b,a)))
            >>> sort
        )
    let dupes = unique S.empty S.empty (map snd names)

    forM_ names $ \(b,a) -> C.putStrLn $ if S.member a dupes
            then a <> " " <> b
            else a

unique :: S.Set C.ByteString -> S.Set C.ByteString -> [C.ByteString] -> S.Set C.ByteString
unique seen dup [] = dup
unique seen dup (x:xs) | S.member x seen = unique seen (S.insert x dup) xs
                       | otherwise       = unique (S.insert x seen) dup xs
