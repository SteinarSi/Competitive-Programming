import           Control.Arrow ((>>>))
import           Data.Char     (toLower)

main :: IO ()
main = getLine >>= (solve >>> putStrLn)

solve :: String -> String
solve "" = ""
solve (x:xs) | toLower x `elem` "aoyeui" = solve xs
             | otherwise = '.' : toLower x : solve xs
