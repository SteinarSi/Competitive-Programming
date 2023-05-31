import Data.Bool (bool)

main :: IO ()
main = interact (bool "first\n" "second\n" . even . read)
