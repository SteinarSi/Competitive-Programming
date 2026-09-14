import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    n <- read <$> getLine
    [0..]
        & find ((2^) >>> (>=n))
        & fromJust
        & print
