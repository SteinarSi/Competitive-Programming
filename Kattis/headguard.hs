import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (lines >>> mapM_ (solve >>> putStrLn))

solve :: String -> String
solve "" = ""
solve xs@(x:_) = show (length a) <> [x] <> solve b
    where (a, b) = span (x==) xs
