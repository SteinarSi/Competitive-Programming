import           Control.Arrow         ((>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:_ <- C.getContents <&> (C.words >>> map readInt)
    putStrLn (batmanacci n k)

batmanacci :: Int -> Int -> String
batmanacci 1 _ = "N"
batmanacci 2 _ = "A"
batmanacci n k | n >= 93 || k <= fibonacci ! (n-2) = batmanacci (n-2) k
               | otherwise                         = batmanacci (n-1) (k - fibonacci ! (n-2))

fibonacci :: UArray Int Int
fibonacci = listArray (1,92) fib
    where fib = 1 : 1 : zipWith (+) fib (tail fib)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
