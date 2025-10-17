{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':rest <- C.getContents <&> C.lines

    let n = readInt n'
        (answers, (r,students)) = splitAt n rest
            & second (drop 1 >>> last &&& (init >>> results n answers))
        comp (n1,s1) (n2,s2) = case r of
            "STUDENT_ID_ASC"  -> compare n1 n2
            "STUDENT_ID_DESC" -> compare n2 n1
            "GRADE_ASC"       -> compare s1 s2 <> compare n1 n2
            "GRADE_DESC"      -> compare s2 s1 <> compare n1 n2

    students
        & sortBy comp
        & map (\(n,s) -> show n <> " " <> show s)
        & unlines
        & putStr

results :: Int -> [C.ByteString] -> [C.ByteString] -> [(Int,Int)]
results n _ [] = []
results n as (i:xs) = splitAt n xs
    & (zipWith (==) as >>> filter id >>> length >>> (readInt i,)) *** results n as
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
