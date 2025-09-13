import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn (atm k xs)

atm :: Int -> [Int] -> String
atm _ [] = ""
atm k (x:xs)
    | k >= x    = '1' : atm (k-x) xs
    | otherwise = '0' : atm k xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
