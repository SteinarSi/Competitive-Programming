import           Control.Arrow ((>>>))

main :: IO ()
main = interact (words >>> last >>> (<>"slop\n"))
