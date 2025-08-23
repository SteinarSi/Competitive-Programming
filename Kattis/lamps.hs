import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [h,p] <- getLine <&> (words >>> map read)

    let days = hoursUntil p (60-5) / h

    print (floor days + 1)

hoursUntil :: Double -> Double -> Double
hoursUntil price offset | until < 1000 = until
                        | otherwise    = 1000 + hoursUntil price (offset + cost 11 - cost 60 - 5)
    where
        cost :: Double -> Double
        cost e = e * price / 100

        until :: Double
        until = max 0 $ (100000 * offset) / (price * (60-11))
