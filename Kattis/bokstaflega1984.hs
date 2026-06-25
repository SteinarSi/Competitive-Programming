import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getLine >>= ((=="1984") >>> bool "Not 1984... yet" "Literally 1984" >>> putStrLn)
