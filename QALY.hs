import Control.Monad (replicateM)

main :: IO ()
main = do
    n <- getLine 
    xs <- replicateM (read n) getLine
    print $ sumYears (map words xs) 0


sumYears :: [[String]] -> Double -> Double 
sumYears [] sum = sum
sumYears ([quality, time]:xs) sum = sumYears xs (sum + read quality * read time)


main' = getLine >>= \n -> mapM (const getLine) [1..read n] >>= print . sum . map (\x -> ( read (words x!!0) :: Float) * (read (words x!!1) :: Float)) 

main'' = getLine >>= flip replicateM (words <$> getLine) . read >>= print . sum . map (\x -> (read (x!!0)) * (read (x!!1) :: Float))
