import           Data.List (intercalate)

main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn ('[' : (intercalate ", " (map (\x -> show (x*x)) [1..n])) <> "]")
