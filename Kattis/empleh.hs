-- See also HelpMeWithTheGame.hs.

import           Control.Applicative (Applicative (liftA2))
import           Data.Char           (ord, toLower, toUpper)
import           Data.Function
import           Data.List           (find, intercalate, intersperse)
import           Data.Maybe          (maybe)

type Piece = ((Char, Int), Char)

main :: IO ()
main = do
    pieces <- liftA2 (++) (readPieces toUpper) (readPieces toLower)
    let squares = map (\row -> map (\col -> formatPiece (col,row) pieces) ['a'..'h']) [8,7..1]
        rows = extersperse "+---+---+---+---+---+---+---+---+" $ map (concat . extersperse "|") squares
    putStrLn $ unlines rows

readPieces :: (Char -> Char) -> IO [Piece]
readPieces f = fmap (concatMap (map parsePiece . splitOn ',') . tail . words) getLine
    where
        parsePiece :: String -> Piece
        parsePiece [col,row]      = ((col,read [row]), f 'p')
        parsePiece [kind,col,row] = ((col,read [row]), f kind)

extersperse :: a -> [a] -> [a]
extersperse a xs = a : intersperse a xs ++ [a]

formatPiece :: (Char, Int) -> [Piece] -> String
formatPiece p@(col,row) pieces = [dotOrColon, piece, dotOrColon]
    where dotOrColon | even (ord col - ord 'a' + row) = '.'
                     | otherwise = ':'
          piece = maybe dotOrColon snd (find ((p ==) . fst) pieces)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
