import Data.Functor ((<&>))

main :: IO ()
main = do
    r <- getLine <&> read
    print (4*cake r r 1 + 1)

cake :: Int -> Int -> Int -> Int
cake r x y | y == r           = 1
           | y^2 + x^2 <= r^2 = 1 + x + cake r x (y+1)
           | otherwise        =         cake r (x-1) y
