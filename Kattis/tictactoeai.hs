import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.Ix       (range)
import           Data.List     (find, transpose)

main :: IO ()
main = do
    [o]:xss <- getContents <&> lines
    let Just (i,j) = find (\(y,x) -> xss !! y !! x == 'E' && win o (play o xss (y,x))) (range ((0,0),(2,2)))
    putStrLn (show (i+1) <> " " <> show (j+1))

play :: Char -> [String] -> (Int,Int) -> [String]
play o xss (y,x) = replace (replace o x (xss!!y)) y xss

replace :: a -> Int -> [a] -> [a]
replace o 0 (_:xs) = o : xs
replace o i (x:xs) = x : replace o (i-1) xs

win :: Char -> [String] -> Bool
win o xss = [o,o,o] `elem` (map (\i -> xss !! i !! i) [0..2] : map (\i -> xss !! i !! (2-i)) [0..2] : xss <> transpose xss)
