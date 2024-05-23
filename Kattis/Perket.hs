import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b)))
        >>> perms
        >>> tail
        >>> map taste
        >>> minimum
        >>> print
    )

taste :: [(Int,Int)] -> Int
taste xs = let sour = map fst xs & product
               bitt = map snd xs & sum
           in  abs (sour - bitt)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = let r = perms xs
               in  r ++ map (x:) r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
