main :: IO ()
main = interact ((\(a:b:_) -> if odd (read b) then "impossible" else "possible") . words)
