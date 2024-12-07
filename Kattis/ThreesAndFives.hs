main :: IO ()
main = do
    n <- read <$> getContents

    let n3 = f n 3
        n5 = f n 5
        n15 = f n 15

    print (n3 + n5 - n15)

f :: Integer -> Integer -> Integer
f n x = x * ((nx*(nx+1)) `div` 2)
    where nx = (n-1) `div` x
