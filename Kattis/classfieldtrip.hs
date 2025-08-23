import Data.List (sort)

main :: IO ()
main = interact (sort . concat . words)