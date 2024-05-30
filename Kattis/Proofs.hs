{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> reverse)
        >>> check S.empty 1
        >>> C.putStrLn
    )

check :: S.Set C.ByteString -> Int -> [[C.ByteString]] -> C.ByteString
check conclusions line [] = "correct"
check conclusions line ((a:"->":assumptions):xs)
    | all (`S.member` conclusions) assumptions = check (S.insert a conclusions) (line+1) xs
    | otherwise = C.pack (show line)
