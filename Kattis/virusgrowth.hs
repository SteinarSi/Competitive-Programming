import           Data.Functor ((<&>))
import           Data.List    (findIndex)

main :: IO ()
main = do
    n <- getLine <&> read
    let Just i = findIndex (>=n) infected
    print i

infected :: [Int]
infected = 0 : 1 : zipWith (+) infected (drop 1 infected)
