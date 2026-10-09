import           Control.Arrow (second, (>>>))
import           Data.Function ((&))
import           Data.List     (isInfixOf)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map parse
        >>> cycle
        >>> take 31
        >>> chunksOf 7
        >>> zipWith (\i xs -> show i <> " " <> xs) [1..5]
        >>> unlines
        >>> putStr
    )

parse :: String -> Char
parse xs
    | "rest" `isInfixOf` xs = '🤖'
    | "leg" `isInfixOf` xs  = '🦿'
    | otherwise             = '🦾'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
