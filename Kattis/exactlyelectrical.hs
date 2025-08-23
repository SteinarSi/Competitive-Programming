import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b,c,d,t] <- getContents <&> (words >>> map read)

    let dist = abs (a-c) + abs (b - d)

    putStrLn $ if t >= dist && even t == even dist
        then "Y"
        else "N"
