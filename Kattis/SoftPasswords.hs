import Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO ()
main = do
    s <- getLine 
    p <- getLine
    if p == s || any ((==s) . (:p)) ['0'..'9'] || any ((==s) . (p++) . pure) ['0'..'9'] || s == swapCase p
        then putStrLn "Yes"
        else putStrLn "No"


swapCase :: String -> String
swapCase "" = ""
swapCase (x:xs) | isLower x = toUpper x : swapCase xs
                | isUpper x = toLower x : swapCase xs
                | otherwise =         x : swapCase xs

