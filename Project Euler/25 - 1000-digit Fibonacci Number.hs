import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.List     (findIndex, scanl')

main :: IO ()
main = do
    let fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
        Just i = findIndex (show >>> length >>> (>=1000)) fibs
    print i
