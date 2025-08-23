import           Control.Arrow ((>>>))
import           Data.Bits     (popCount)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    print (popCount n)
