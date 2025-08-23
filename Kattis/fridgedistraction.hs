import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (intersperse)
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    [n,t] <- getLine <&> (words >>> map read)

    let ans = take n ['a'..]
            & reverse
            & Seq.fromList
            & distract n t

    putStrLn (show (length ans) <> "\n" <> intersperse ' ' ans)

distract :: Int -> Int -> Seq Char -> [Char]
distract n t Empty = error "bruh"
distract n t (x :<| xs) = case compare n t of
    LT -> x : distract n (t-n) (xs :|> x)
    EQ -> [x]
    GT -> distract (n-1) t xs
