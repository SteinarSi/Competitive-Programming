import           Data.Char     (chr, digitToInt, ord)
import           Data.Function ((&))
import           Data.Ix       (inRange)
import           Data.List     (sort)

main :: IO ()
main = do
    [c,r] <- getLine

    let (x,y) = (ord c - ord 'a' + 1, digitToInt r)

    [(x+1,y+2),(x+2,y+1),(x+2,y-1),(x+1,y-2),(x-1,y-2),(x-2,y-1),(x-2,y+1),(x-1,y+2)]
        & filter (inRange ((1,1),(8,8)))
        & map (\(i,j) -> chr (i + ord 'a' - 1) : show j)
        & sort
        & unlines
        & putStr
