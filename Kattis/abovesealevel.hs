import           Control.Arrow ((>>>))

main :: IO ()
main = interact (read >>> subtract 0.3 >>> show)
