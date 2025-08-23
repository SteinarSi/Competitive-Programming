import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet           as S
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map parseSpec
        >>> foldl' (\(r,s) p -> (r + length p, S.union s (S.fromAscList p))) (0,S.empty)
        >>> (\(r,s) -> show (S.size s) <> " " <> show r)
        >>> putStrLn
    )

parseSpec :: String -> [Int]
parseSpec xs = do
    let [h,m,s] = map (splitOn ',' >>> map (splitOn '-')) (splitOn ' ' xs)
    hh <- parseSection 23 h
    mm <- parseSection 59 m
    ss <- parseSection 59 s
    pure (60 * 60 * hh + 60 * mm + ss)
  where
    parseSection :: Int -> [[String]] -> [Int]
    parseSection _ []         = []
    parseSection r [["*"]]    = [0..r]
    parseSection r ([x]:ys)   = read x : parseSection r ys
    parseSection r ([x,y]:ys) = [read x .. read y] <> parseSection r ys

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
