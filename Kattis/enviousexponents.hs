import           Control.Arrow ((>>>))
import           Data.Bits     (complement, popCount, (.&.), (.|.))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)
    print (solve k (n+1))

solve :: Int -> Int -> Int
solve k n = case compare p k of
        LT -> solve k (n .|. (n+1))
        EQ -> n
        GT -> solve k (n + (n .&. complement (n-1)))
    where
        p = popCount n
