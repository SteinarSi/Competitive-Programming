import           Control.Arrow (first, (>>>), (&&&))
import           Text.Printf   (printf)

main :: IO ()
main = getLine >>= (
            read 
        >>> digits 
        >>> length &&& sum
        >>> (uncurry smol >>> format)
            &&&
            (uncurry bigg >>> format)
        >>> uncurry (printf "%s %s\n")
    )

smol :: Int -> Int -> [Int]
smol len tot = b+1 : bs
    where b:bs = reverse (bigg len (tot-1))

bigg :: Int -> Int -> [Int]
bigg 0 _ = []
bigg 1 tot = [tot]
bigg len tot = m : bigg (len-1) (tot-m)
    where m = min tot 9

format :: [Int] -> String
format = concatMap show

digits :: Int -> [Int]
digits 0 = []
digits x = let (q,r) = quotRem x 10
           in  r : digits q
