import Data.Bool (bool)

main :: IO ()
main = fmap (bool "Alice" "Bob" . even . read) getLine >>= putStrLn