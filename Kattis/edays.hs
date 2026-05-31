import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> (*2) >>> print)
