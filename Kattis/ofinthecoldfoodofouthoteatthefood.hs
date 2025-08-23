import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [t,h] <- getLine <&> (words >>> map read)

    let ans | total >= tri = h + (total-tri) / h
            | otherwise    = sqrt (2*total)

        total = h*t
        tri = h^2/2

    print $ if total >= tri
        then h + (total-tri) / h
        else sqrt (2*total)
