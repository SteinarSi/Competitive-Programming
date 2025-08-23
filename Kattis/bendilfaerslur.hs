import           Control.Arrow (first, (>>>))
import           Data.Function ((&))
import           Data.List     (intercalate, intersperse)

main :: IO ()
main = do
    xs <- getLine
    putStrLn $ if '.' `elem` xs
        then parseIPv4 xs
        else parseIPv6 xs

parseIPv4 :: String -> String
parseIPv4 = splitOn '.'
        >>> reverse
        >>> intercalate "."
        >>> (<> ".in-addr.arpa.")

parseIPv6 :: String -> String
parseIPv6 xs = parts
        & map ((\x -> replicate (4-length x) '0' <> x) >>> reverse)
        & reverse
        & concat
        & intersperse '.'
        & (<> ".ip6.arpa.")
    where
        parts = case splitDouble xs of
            Nothing    -> splitOn ':' xs
            Just (a,b) -> let (a',b') = (splitOn ':' a, splitOn ':' b)
                          in  a' <> replicate (8 - length a' - length b') "0000" <> b'

splitDouble :: String -> Maybe (String,String)
splitDouble ""           = Nothing
splitDouble [x]          = Nothing
splitDouble (':':':':xs) = Just ("", xs)
splitDouble (x:xs)       = fmap (first (x:)) (splitDouble xs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
