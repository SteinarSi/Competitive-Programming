import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.List     (isInfixOf)

main :: IO ()
main = interact (("kth" `isInfixOf`) >>> bool "no" "yes")
