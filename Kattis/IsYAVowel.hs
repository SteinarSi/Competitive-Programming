import           Control.Arrow  ((>>>))
import           Data.Bifunctor (bimap, second)

main :: IO ()
main = getLine >>= (
            vowels
        >>> (\(a,b) -> show a ++ " " ++ show b)
        >>> putStrLn
    )

vowels :: String -> (Int, Int)
vowels "" = (0,0)
vowels ('y':xs)                  = second succ (vowels xs)
vowels (x:xs) | x `elem` "aeiou" = bimap succ succ (vowels xs)
              | otherwise        = vowels xs
