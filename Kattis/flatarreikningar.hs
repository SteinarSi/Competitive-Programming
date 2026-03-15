main :: IO ()
main = do
    v <- getLine
    let r = (3*read v / (4*pi)) ** (1/3)
    print (4*pi*r^2)
