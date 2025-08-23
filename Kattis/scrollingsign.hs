import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> split
        >>> map (uncurry solve >>> show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> [C.ByteString] -> Int
solve k [_]      = k
solve k (x:y:xs) = (k-cost) + solve k (y:xs)
    where
        cost = [k,k-1..0]
            & find (\j -> C.takeEnd j x == C.take j y)
            & fromJust

split :: [C.ByteString] -> [(Int,[C.ByteString])]
split [] = []
split (x:xs) = let [k,w] = map readInt (C.words x)
                   (a,b) = splitAt w xs
               in  (k,a) : split b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
