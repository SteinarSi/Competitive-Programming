import Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (score (0,0) >>> putStrLn)

score :: (Int,Int) -> String -> String
score (t,h) xs | win t h || win h t = score (0,0) xs
score (t,h) ('T':xs) = score (t+1,h) xs
score (t,h) ('H':xs) = score (t,h+1) xs
score (t,h) [] = show t <> "-" <> show h

win :: Int -> Int -> Bool
win a b = a > 10 && a-b >= 2
