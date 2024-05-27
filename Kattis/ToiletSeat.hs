import           Data.Bool (bool)

alwaysUp :: Char -> Char -> Int
alwaysUp 'U' 'U' = 0
alwaysUp 'D' 'D' = 1
alwaysUp 'U' 'D' = 2
alwaysUp  _   _  = 1

alwaysDown :: Char -> Char -> Int
alwaysDown 'U' 'U' = 1
alwaysDown 'D' 'D' = 0
alwaysDown 'U' 'D' = 1
alwaysDown  _   _  = 2

justLeaveAsIs :: Char -> Char -> Int
justLeaveAsIs = (bool 1 0 .) . (==)

main :: IO ()
main = do
    x:xs <- getLine

    print . sum $ zipWith alwaysUp   (x : repeat 'U') xs
    print . sum $ zipWith alwaysDown (x : repeat 'D') xs
    print . sum $ zipWith justLeaveAsIs (x : xs) xs
