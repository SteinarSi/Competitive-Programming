import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Text.Printf   (printf)

main :: IO ()
main = interact (read >>> solve >>> (++"\n"))

solve :: Int -> String
solve = succ >>> printf "%b" >>> tail >>> map ((=='1') >>> bool '4' '7')
