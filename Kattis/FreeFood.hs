main :: IO ()
main = getLine >>= getIntervals . read >>= print . length

getIntervals :: Int -> IO [Int]
getIntervals 0 = return []
getIntervals n = do
    x <- getLine
    xs <- getIntervals (n-1)
    return ([y | y <- [fss x .. snn x], not $ elem y xs] ++ xs)
    where fss x = read (words x !! 0) ::Int
          snn x = read (words x !! 1) ::Int