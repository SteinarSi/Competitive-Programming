import           Control.Monad (when)
import           Data.Ratio    (denominator, numerator)

main :: IO ()
main = do
    [numerator, denominator] <- fmap (map read . words) getLine
    when (denominator > 0) $ do
        let (qotient, remainder) = quotRem numerator denominator
        putStrLn $ unwords [show qotient, show remainder, "/", show denominator]
        main
