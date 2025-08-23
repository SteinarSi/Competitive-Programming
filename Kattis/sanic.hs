import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> subtract 1.0 >>> print)
