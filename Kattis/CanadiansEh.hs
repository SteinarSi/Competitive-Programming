import           Data.Bool (bool)
import           Data.List (isSuffixOf)

main :: IO ()
main = getLine >>= putStrLn . bool "Imposter!" "Canadian!" . isSuffixOf "eh?"
