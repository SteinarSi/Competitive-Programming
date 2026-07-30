import           Data.Bits (popCount)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Integer
    putStrLn $ if popCount n == 1
        then "Yes"
        else "No"
