import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [l,n,m] <- getLine <&> (words >>> map read)
    let k = minimum [l, n, m]
        layout = cycle >>> take l >>> map show >>> unwords >>> putStrLn
    if k <= 1 || odd l && k <= 2
        then print (-1)
        else do
            print k
            layout [1..k]
            layout ([2..k] <> [1])
