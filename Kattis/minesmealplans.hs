import           Control.Arrow ((>>>))
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [String] -> String
solve [n,x,y,z] = unwords [n, show rs, printf "%.2f" rm, c]
  where
    Just (s,m) = lookup x [
            ("Marble",(19,200)),
            ("Marble+",(19,350)),
            ("Quartz",(14,200)),
            ("Quartz+",(14,350))
        ]
    c = case (rs > 0, rm > 0) of
        (False,False) -> "Go to Downtown Golden!"
        (True,False)  -> "Use meal swipe"
        (False,True)  -> "Use munch money"
        (True,True)   -> "Use meal swipe or munch money"
    rs = s - read y :: Int
    rm = m - read z :: Double
