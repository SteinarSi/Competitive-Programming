import           Data.Function   ((&))
import           Data.Functor    ((<&>))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    _:xs <- getContents <&> lines
    M.fromListWith (+) (map (,1) xs)
        & M.elems
        & foldl' (\r x -> r `div` factorial x) (factorial (length xs))
        & print

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)
