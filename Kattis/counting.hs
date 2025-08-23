import           Control.Arrow ((>>>))

main :: IO ()
main = interact (read >>> enumFromThen 0 >>> drop 1 >>> take 12 >>> map show >>> unlines)
