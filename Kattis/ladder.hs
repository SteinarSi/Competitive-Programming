main :: IO ()
main = interact (show . ceiling . (\(h:v:_) -> h / sin (0.01745329 * v)) . map read . words) >> putChar '\n'