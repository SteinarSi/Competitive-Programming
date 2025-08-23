import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    p <- getLine <&> read
    takeWhile (<=p) fortytwos
        & filter (mod p >>> (==0))
        & map show
        & unlines
        & putStr

fortytwos :: [Int]
fortytwos = 2 : 4 : concatMap (\x -> [10*x + 2, 10*x + 4]) fortytwos
