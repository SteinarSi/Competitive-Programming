import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readInt
            >>> \[i,x,y] -> format i (solve (x,y)))
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int) -> Maybe [Int]
solve (x,y) | x < y  = Just [x,y]
            | y <= 3 = Nothing
            | otherwise = Just [1,2,3,x-y+5,x+2,x+3]

format :: Int -> Maybe [Int] -> String
format i = maybe (show i <> " NO PATH") (\xs -> unwords (map show (i : length xs : xs)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
