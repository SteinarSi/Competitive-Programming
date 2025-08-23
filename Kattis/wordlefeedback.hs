import           Data.Function ((&))
import           Data.List     (delete)

main :: IO ()
main = do
    secret <- getLine
    guess  <- getLine
    let rem = zip secret guess
            & filter (uncurry (/=))
            & map fst
    putStrLn (feedback rem secret guess)

feedback :: String -> String -> String -> String
feedback rem "" "" = ""
feedback rem (x:xs) (y:ys) | x == y       = 'G' : feedback rem xs ys
                           | y `elem` rem = 'Y' : feedback (delete y rem) xs ys
                           | otherwise    = '-' : feedback rem xs ys
