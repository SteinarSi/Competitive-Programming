import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [num,_,den] <- getContents <&> (words >>> map read)

    let d | den < 0   = negate (gcd num den)
          | otherwise = gcd num den

    if num `mod` den /= (0 :: Int)
        then printf "%d/%d\n" (num `div` d) (den `div` d)
        else print (num `div` den)
