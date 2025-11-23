import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> (last >>> (3*) >>> (0,))
                &&&
            (init >>> sort >>> map (,1))
        >>> uncurry (:)
        >>> play
        >>> map show
        >>> unwords
        >>> putStrLn
    )

play :: [(Int,Int)] -> [Int]
play [] = []
play ((x,i):(y,j):xs) | x == y = play ((x,i+j):xs)
play ((x,i):xs) = case quotRem i 2 of
    (0,0) ->     play xs
    (0,1) -> x : play xs
    (q,0) ->     play ((x+1,q):xs)
    (q,1) -> x : play ((x+1,q):xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
