import Data.IntSet (IntSet, fromList, size)

main :: IO ()
main = interact (show . size . fromList . map ((`mod` 42) . read) . words) >> putChar '\n'

