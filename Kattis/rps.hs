import           Control.Arrow ((>>>))
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> parse
        >>> concatMap (solve (0,0))
        >>> putStr
    )

wins :: [(Char,Char)]
wins = [('R','S'),('S','P'),('P','R')]

solve :: (Int,Int) -> (String,String) -> String
solve (a,b) (x:xs,y:ys)
    | (x,y) `elem` wins = solve (a+1,b) (xs,ys)
    | (y,x) `elem` wins = solve (a,b+1) (xs,ys)
    | otherwise         = solve (a,b) (xs,ys)
solve (a,b) _ = printf "P1: %d\nP2: %d\n" a b

parse :: [String] -> [(String,String)]
parse []          = []
parse [_]         = []
parse [_,_]       = []
parse (xs:ys:xss) = (xs,ys) : parse xss
