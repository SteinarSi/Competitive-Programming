import           Control.Arrow ((>>>))
import           Data.Char     (isDigit)
import           Data.Function (on)
import           Data.Functor  ((<&>))
import           Data.List     (groupBy)

main :: IO ()
main = do
    [x,_,y,_,z] <- getLine <&> (groupBy ((==) `on` isDigit) >>> map read)

    let (q,r) = quotRem (x * (y+1)) 2

    putStrLn $ if r == 0
        then show (q+z)
        else show (q+z) <> ".5"
