import           Control.Arrow  ((***), (>>>))
import           Data.Bifunctor (bimap)
import           Data.Char      (chr, ord)

main :: IO ()
main = getLine >>= (divide >>> rotate *** rotate >>> merge >>> putStrLn)

divide :: String -> (String, String)
divide xs = splitAt (length xs `div` 2) xs

rotate :: String -> String
rotate xs = map (`rotateChar` rotationalValue) xs
    where rotationalValue = sum $ map value xs

merge :: (String, String) -> String
merge = uncurry (zipWith ((. value) . rotateChar))

value :: Char -> Int
value c = ord c - ord 'A'

rotateChar :: Char -> Int -> Char
rotateChar c x = chr ((ord c - ord 'A' + x) `mod` 26 + ord 'A')
