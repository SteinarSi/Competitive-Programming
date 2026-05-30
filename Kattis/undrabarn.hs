import           Control.Arrow ((>>>))
import           Data.Char     (intToDigit)

main :: IO ()
main = getLine >>= (read >>> solulu >>> putStrLn)

solulu :: Int -> String
solulu k = map con (reverse (digits k))
  where
    con 7 = '9'
    con x = intToDigit (x+1)

digits :: Int -> [Int]
digits 0 = []
digits x = let (q,r) = quotRem (x-1) 8
           in  r : digits q
