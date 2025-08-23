import           Control.Applicative   ((<|>))
import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (intercalate)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInt >>> solve)
        >>> intercalate "\n\n"
        >>> putStrLn
    )

solve :: Int -> String
solve w = distribute ([],[]) w threes
    & fromJust
    & (map show >>> unwords) *** (map show >>> unwords)
    & uncurry (printf "left pan: %s\nright pan: %s")
  where
    distribute :: ([Int],[Int]) -> Int -> [Int] -> Maybe ([Int],[Int])
    distribute (ls,rs) 0 _      = Just (reverse ls, reverse rs)
    distribute _ _ []           = Nothing
    distribute (ls,rs) c (x:xs) | c < 0     = distribute (ls,rs) c xs <|> distribute (x:ls,rs) (c+x) xs
                                | otherwise = distribute (ls,rs) c xs <|> distribute (ls,x:rs) (c-x) xs

threes :: [Int]
threes = map (3^) [20,19..0]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
