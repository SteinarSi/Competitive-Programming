import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:p:xs <- C.getContents <&> (C.words >>> map readInt)

    putStrLn $ if p == 100
        then "impossible"
        else show (evaluate n p (sum xs))

evaluate :: Int -> Int -> Int -> Int
evaluate n p zum | zum == n * p            = 0
                 | (n+1) * p - zum >= 100  = succ $ evaluate (n+1) p (zum + 100)
                 | otherwise               = 1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
