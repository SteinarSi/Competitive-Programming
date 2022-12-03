import Control.Applicative (liftA2)
import Data.Bool (bool)

main = liftA2 (\x y -> length x >= length y) getLine getLine >>= putStrLn . bool "no" "go" 