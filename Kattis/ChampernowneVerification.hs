import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            read
        >>> digits
        >>> champerowne 0
        >>> print
    )

champerowne :: Int -> [Int] -> Int
champerowne ret [] = ret
champerowne ret xs | and (zipWith (==) a ds) = champerowne (ret+1) b
                   | otherwise = -1
    where ds = digits (ret+1)
          (a,b) = splitAt (length ds) xs

digits :: Int -> [Int]
digits = digits' >>> reverse
    where

        digits' 0 = []
        digits' x = x `mod` 10 : digits' (x `div` 10)
