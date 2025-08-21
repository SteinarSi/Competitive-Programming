import           Data.Bits    (popCount)
import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    print (2^popCount (n::Int))
