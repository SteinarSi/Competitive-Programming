import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (words >>> map read >>> sum >>> fromIntegral >>> (/2) >>> print)
