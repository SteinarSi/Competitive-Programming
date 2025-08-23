import           Data.List  (find)
import           Data.Maybe (listToMaybe)

main :: IO ()
main = do
    a:op:b:_:c:_ <- fmap words getLine
    let (x, y, z) = solve (readOp op) (read a) (read b) (read c)
    putStrLn $ unwords [show x, op, show y, "=", show z]

solve :: (Int -> Int -> Int) -> Int -> Int -> Int -> (Int, Int, Int)
solve op a b c = head $ concat [
        [ (x,y,c) | (x,y) <- spoon a b, x `op` y == c ],
        [ (x,b,z) | (x,z) <- spoon a c, x `op` b == z ],
        [ (a,y,z) | (y,z) <- spoon b c, a `op` y == z ]
    ]
    where
        spoon :: Int -> Int -> [(Int, Int)]
        spoon a b = [swap i j | i <- [1..n-1], j <- [1..m-1]]
            where
                n = len a
                m = len b

                swap :: Int -> Int -> (Int, Int)
                swap i j = (
                        a `mod` (10^(n-i)) + (b `div` (10^(m-j))) * 10^(n-i),
                        b `mod` (10^(m-j)) + (a `div` (10^(n-i))) * 10^(m-j)
                    )

readOp :: String -> (Int -> Int -> Int)
readOp "+" = (+)
readOp "*" = (*)

len :: Int -> Int
len 0 = 0
len x = 1 + len (x `div` 10)
