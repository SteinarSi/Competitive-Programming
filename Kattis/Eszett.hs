import           Control.Arrow ((>>>))
import           Data.Char     (toLower)

main :: IO ()
main = interact (init >>> solve >>> unlines)

solve :: String -> [String]
solve ""           = [""]
solve ('S':'S':xs) = map ('s':) (solve ('S':xs)) <> map ('B':) (solve xs)
solve (x:xs)       = map (toLower x :) (solve xs)
