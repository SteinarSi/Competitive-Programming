import           Data.List (sort)

main :: IO ()
main = getContents >>= putStrLn . unwords . ("1":) . map (show . snd) . sort . flip zip [2..] . map (read::String->Int) . tail . words
