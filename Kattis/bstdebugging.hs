import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:m:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn $ if search m (minBound,maxBound) xs
        then "valid"
        else "invalid"

search :: Int -> (Int,Int) -> [Int] -> Bool
search m (lo,hi) [] = True
search m (lo,hi) (x:xs) = inRange (lo,hi) x && case compare m x of
    EQ -> null xs
    LT -> search m (lo,x-1) xs
    GT -> search m (x+1,hi) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
