import           Data.Functor ((<&>))

main :: IO ()
main = do
    [x1,y1,x2,y2,x3,y3] <- getContents <&> words

    let x | x1 == x2  = x3
          | x1 == x3  = x2
          | otherwise = x1
        y | y1 == y2  = y3
          | y1 == y3  = y2
          | otherwise = y1

    putStrLn (x <> " " <> y)
