-- See also EmagEhtHtiwEmPle.hs.

import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Char     (isLower, isUpper, toUpper)
import           Data.Function (on)
import           Data.List     (elemIndex, intercalate, sortBy)
import           Data.Maybe    (fromMaybe)

type Piece = ((Char, Int), Char)

main :: IO ()
main = do
    pieces <- fmap (parse >>> sortPieces) getContents
    putStrLn (formatPieces "White" isUpper pieces)
    putStrLn (formatPieces "Black" isLower pieces)

parse :: String -> [Piece]
parse = lines
    >>> filter (('+' /=) . head)
    >>> concatMap (map (filter (`notElem` ".:")) . splitOn '|')
    >>> zip [(c, r) | r <- [8,7..1], c <- ['a'..'h']]
    >>> filter (not . null . snd)
    >>> map (fmap head)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)

sortPieces :: [Piece] -> [Piece]
sortPieces = sortBy (compare `on` (\((col,row), kind) -> (
        fromMaybe 99 (elemIndex (toUpper kind) "KQRBNP"),
        bool row (negate row) (isLower kind),
        col
    )))

formatPiece :: Piece -> String
formatPiece ((col,row), p) = kind ++ col : show row
    where kind | p `elem` "Pp" = ""
               | otherwise     = [toUpper p]

formatPieces :: String -> (Char -> Bool) -> [Piece] -> String
formatPieces color pred pieces = color ++ ": " ++ intercalate "," (map formatPiece $ filter (pred . snd) pieces)
