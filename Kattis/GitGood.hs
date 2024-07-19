{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map C.words
        >>> git S.empty [""]
        >>> map ("git add " <>)
        >>> C.unlines
        >>> C.putStr
    )
    >> C.putStrLn "git commit\ngit push"

git :: S.Set C.ByteString -> [C.ByteString] -> [[C.ByteString]] -> [C.ByteString]
git ret _       []                   = S.toAscList ret
git ret (_:ps) (["cd"  , ".."  ]:xs) = git ret ps xs
git ret (p:ps) (["cd"  , folder]:xs) = git ret (p <> folder <> "/" : p : ps) xs
git ret (p:ps) (["nano", file  ]:xs) = git (S.insert (p <> file) ret) (p:ps) xs
