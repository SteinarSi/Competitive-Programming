import           Data.Char (isAlpha)

main :: IO ()
main = interact (filter isAlpha)
