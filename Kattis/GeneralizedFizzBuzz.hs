import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.Function ((&))

main :: IO ()
main = do
    [n,a,b] <- getContents <&> (words >>> map read)

    let fizz = (n `div` a) - fizzbuzz
        buzz = (n `div` b) - fizzbuzz
        fizzbuzz = n `div` lcm a b

    [fizz,buzz,fizzbuzz]
        & map show
        & unwords
        & putStrLn
