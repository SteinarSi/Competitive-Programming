import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let ks = filter (valid n xs) (factors n)
    if null ks
        then putStrLn "-1"
        else mapM_ print ks

valid :: Int -> [Int] -> Int -> Bool
valid n xs k = and $ zipWith (\x y -> maximum x < minimum y) g (tail g)
    where g = chunksOf (n `div` k) xs

factors :: Int -> [Int]
factors n = filter (mod n >>> (0==)) [2 .. (n+1)`div` 2] ++ [n]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = a : chunksOf k b
    where (a,b) = splitAt k xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
