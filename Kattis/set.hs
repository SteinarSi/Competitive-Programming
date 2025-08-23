import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.List     (intercalate, sort)

main :: IO ()
main = getContents >>= (
            words
        >>> zip [1..]
        >>> set
        >>> sort
        >>> format
        >>> putStrLn
    )

set :: [(Int,String)] -> [[Int]]
set [] = []
set ((x,c1):xs) = set' xs ++ set xs
    where
        set' [] = []
        set' ((y,c2):ys) = set' ys ++ do
            (z,c3) <- ys
            guard (zipWith (==) c1 c2 == zipWith (==) c1 c3)
            guard (zipWith (==) c1 c2 == zipWith (==) c2 c3)
            pure [x,y,z]

format :: [[Int]] -> String
format [] = "no sets"
format xs = intercalate "\n" (map (map show >>> unwords) xs)
