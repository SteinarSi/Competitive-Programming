main :: IO ()
main = getLine >>= putChar . last
