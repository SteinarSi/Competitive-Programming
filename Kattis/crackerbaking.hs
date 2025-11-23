import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> fromInteger >>> sqrt >>> ceiling >>> print)
