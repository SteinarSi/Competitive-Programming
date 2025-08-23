import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (words >>> last >>> solve >>> print)

solve :: String -> Int
solve "" = 0
solve (x:xs) = let (a,b) = span (x==) xs
               in  length a + solve b
