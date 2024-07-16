import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (delete, sort)

main :: IO ()
main = do
    _:p:xs <- getContents <&> (words >>> map read)

    let x = xs !! p
        order = x : sort (delete x xs)
        (c, pen) = solve 0 0 0 order

    putStrLn (show c ++ " " ++ show pen)

solve :: Int -> Int -> Int -> [Int] -> (Int, Int)
solve c pen time [] = (c, pen)
solve c pen time (x:xs) | time' > 300 = (c, pen)
                        | otherwise   = solve (c+1) (pen + time') time' xs
    where time' = time + x
