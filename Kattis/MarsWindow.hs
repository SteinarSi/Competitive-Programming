import           Data.Functor ((<&>))

main :: IO ()
main = do
    year <- getLine <&> read
    putStrLn $ if year `elem` takeWhile (<=year) years
        then "yes"
        else "no"

years :: [Int]
years = map fst $ iterate next (2018,3)

next :: (Int,Int) -> (Int,Int)
next (year, month) = (year + q, r)
    where (q, r) = quotRem (month + 26) 12
