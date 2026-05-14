import           Data.Functor ((<&>))
import           Data.List    (find)
import           Text.Printf  (printf)

main :: IO ()
main = do
    n <- getLine <&> read
    let Just i = find (\j -> ((j+1)*j) `div` 2 >= n) [1::Int ..]
        r = n - ((i-1)*i) `div` 2 - 1
        d = gcd i r
    if r == 0
        then print i
        else printf "%d %d/%d\n" i (r `div` d) (i `div` d)
