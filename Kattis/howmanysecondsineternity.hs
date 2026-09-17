import           Control.Arrow ((>>>))

main :: IO ()
main = interact (read >>> (*(40*60*60)) >>> show)
