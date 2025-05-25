import           Control.Arrow ((>>>))
import           Data.Bits     (popCount)

main :: IO ()
main = getLine >>= (words >>> last >>> (read::String->Int) >>> popCount >>> print)
