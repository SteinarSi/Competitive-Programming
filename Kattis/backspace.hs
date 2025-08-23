main :: IO ()
main = getLine >>= putStrLn . reverse . backspace 0 . reverse

backspace :: Int -> String -> String
backspace _ "" = ""
backspace n ('<':xs) = backspace (succ n) xs
backspace 0 (x:xs) = x : backspace 0 xs
backspace n (x:xs) = backspace (pred n) xs
