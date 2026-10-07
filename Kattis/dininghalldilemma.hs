import           Control.Arrow ((>>>))
import           Data.Bits     (shiftL, testBit)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n:k:d:ks <- getContents <&> (words >>> map read)

    let forced = sum (map (pred >>> (1 `shiftL`)) ks)
        missing = d - forced
        possible = all (\b -> not (testBit missing b) || (b > 0 && not (testBit forced (b-1)))) [0..n+1]

    print $ if missing <= 1 `shiftL` (n+1) && missing >= 0 && possible
        then (1::Int) `shiftL` k
        else 0
