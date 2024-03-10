main :: IO ()
main = getLine >>= putChar . head
