import Data.Char (ord, chr)

main :: IO ()
main = interact (unlines . map ((\(r:w:_) -> map (\c -> chrr ((ordd c + read r) `mod` 28)) (reverse w)) . words) . init . lines)

ordd :: Char -> Int
ordd '_' = 26
ordd '.' = 27
ordd  c  = ord c - 65

chrr :: Int -> Char
chrr 26 = '_'
chrr 27 = '.'
chrr o  = chr (o + 65)
