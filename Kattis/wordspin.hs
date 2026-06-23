import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys] <- C.getLine <&> C.words

    C.zipWith (\x y -> ord x - ord y) xs ys
        & shift 0
        & print

shift :: Int -> [Int] -> Int
shift _ [] = 0
shift s (x:xs)
    | compare s 0 /= compare x 0 = abs x + shift x xs
    | abs s >= abs x = shift x xs
    | otherwise = abs (x-s) + shift x xs
