import           Control.Arrow ((&&&), (>>>))
import           Data.Function ((&))

main :: IO ()
main = interact (read >>> build 'a')

build :: Char -> Int -> String
build _ 0 = ""
build c n = replicate a c <> build (succ c) (n-m)
  where
    (a,m) = [1..]
        & map (id &&& (\x -> (x*(x+1)) `div` 2))
        & takeWhile (snd >>> (<=n))
        & last
