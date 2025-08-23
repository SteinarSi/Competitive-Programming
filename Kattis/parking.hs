import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (sort)

main :: IO ()
main = do
    costs <- getLine <&> (words >>> map read >>> (0:))
    times <- getContents <&> (
            lines
        >>> concatMap (
                words
            >>> map read
            >>> (\(a:b:_) -> [(a,1),(b,-1)])
        )
        >>> sort
        )

    print (parking 0 0 costs times)

parking :: Int -> Int -> [Int] -> [(Int,Int)] -> Int
parking curr prev costs [] = 0
parking curr prev costs ((a,d):xs) = curr * (costs !! curr) * (a - prev) + parking (curr+d) a costs xs
