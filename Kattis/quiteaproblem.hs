import Data.Char (toLower)

main :: IO ()
main= interact (unlines . map (problem . map toLower) . lines)

problem :: String -> String
problem [] = "no"
problem xs | take 7 xs == "problem" = "yes"
           | otherwise = problem (tail xs)
