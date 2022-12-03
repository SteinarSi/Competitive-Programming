import Data.Char

count :: Char->String->Int
count _ [] = 0;
count c (x:xs) = if x == c then (count c xs) +1
                else count c xs
foo :: String -> Int
foo xs = count ' ' $ bar xs 0

bar ::  String -> Int -> String
bar [] _ = []
bar (x:xs) n = if  (isUpper x) && (mod n 4 /= 0) then " "++bar (x:xs) (n+1)
else x:(bar xs (n+1))

main :: IO ()
main = do 
    input<-getLine
    print(foo input)