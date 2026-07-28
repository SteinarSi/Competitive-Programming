import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (words >>> map (read::String->Int) >>> maximum >>> print)
