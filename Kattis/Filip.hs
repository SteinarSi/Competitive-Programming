main :: IO ()
main = interact (maximum . map reverse . words) >> putChar '\n'