import           Data.List (splitAt)

main :: IO ()
main = do
    inn <- getLine
    putStrLn $ minimum [ lektira inn (p, q) | p <- [1..length inn-2], q <- [p+1..length inn-1] ]

lektira :: String -> (Int, Int) -> String
lektira s (p, q) = reverse first ++ reverse second ++ reverse third
    where (first, xs) = splitAt p s
          (second, third) = splitAt (q-p) xs
