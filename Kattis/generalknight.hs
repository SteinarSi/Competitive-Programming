import           Data.Char     (chr, digitToInt, ord)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Ix       (inRange)
import           Data.List     (nub, sort)

main :: IO ()
main = do
    [a',b',[c,r]] <- getContents <&> words

    let a = read a'
        b = read b'
        (x,y) = (ord c - ord 'a' + 1, digitToInt r)

        combs = do
            p <- [a,-a]
            q <- [b,-b]
            [(x+p,y+q),(x+q,y+p)]

        squares = filter (inRange ((1,1),(8,8))) combs
            & nub
            & sort
            & map (\(c,r) -> chr (c + ord 'a' - 1) : show r)

    print (length squares)
    putStrLn (unwords squares)
