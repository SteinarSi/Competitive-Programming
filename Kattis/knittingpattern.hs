import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,p] <- getContents <&> (words >>> map read)

    let left = n - p
        left' = left - 2*p*(left `div` (2*p))
        left'' | even p && left' == p = 0
               | otherwise = left'

    print left''
