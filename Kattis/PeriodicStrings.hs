import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.List     (find)

main :: IO ()
main = do
    s <- getLine

    let n = length s
        k = head $ do
            h <- [1..n`div`2] ++ [n]
            guard (mod n h == 0)
            guard (zipWith (==) (chunksOf h s) (iterate rotate (take h s)) & and)
            pure h

    print k

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)

rotate :: String -> String
rotate xs = last xs : init xs
