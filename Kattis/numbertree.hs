import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha, isDigit)
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    (h,xs) <- getContents <&> (span isDigit >>> read *** filter isAlpha)
    print (2^(h+1) - 2^(length xs + 1) + parse 0 (reverse xs))

parse :: Int -> String -> Int
parse _ ""       = 1
parse k ('L':xs) = 2^k + parse (succ k) xs
parse k ('R':xs) =       parse (succ k) xs
parse k (_:xs)   =       parse k xs
