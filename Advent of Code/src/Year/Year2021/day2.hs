
main :: IO ()
main = do
    s <- readFile "day2-input.txt"
    let (h, d) = navigate (0, 0) $ lines s
    print (h * abs d)
    let (h2, d2) = navigateWithAim (0, 0) 0 $ lines s
    print (h2 * d2)

navigate :: (Int, Int) -> [String] -> (Int, Int)
navigate (h, d) [] = (h, d)
navigate (h, d) (c:cs) = case words c of
    ("down"   :x:_) -> navigate (h, d - read x) cs
    ("up"     :x:_) -> navigate (h, d + read x) cs
    ("forward":x:_) -> navigate (h + read x, d) cs
    _ -> undefined


navigateWithAim :: (Int, Int) -> Int -> [String] -> (Int, Int)
navigateWithAim (h, d) aim [] = (h, d)
navigateWithAim (h, d) aim (c:cs) = case words c of
    ("down"   :x:_) -> navigateWithAim (h, d) (aim + read x) cs
    ("up"     :x:_) -> navigateWithAim (h, d) (aim - read x) cs
    ("forward":x:_) -> navigateWithAim (h + read x, d + read x * aim) aim cs
    _ -> undefined