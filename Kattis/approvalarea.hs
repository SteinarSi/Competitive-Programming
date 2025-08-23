import           Control.Arrow ((>>>))

main :: IO ()
main = interact (read >>> sqrt >>> (^2) >>> (*pi) >>> show)
