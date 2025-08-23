import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    print (sum (map (signs n) [0..9]))

signs :: Int -> Int -> Int
signs n 0 = max 1 (length (show n) - 1)
signs n d = case takeWhile (<=n) (repeating d) of
    [] -> 0
    ds -> length (show (last ds))

repeating :: Int -> [Int]
repeating x = x : map ((10*) >>> (+x)) (repeating x)
