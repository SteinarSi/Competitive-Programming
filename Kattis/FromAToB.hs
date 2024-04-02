import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a, b] <- getLine <&> (words >>> map read)
    print (search b a)

search :: Int -> Int -> Int
search goal curr | goal >= curr = goal - curr
                 | odd curr     = 1 + search goal (curr + 1)
                 | even curr    = 1 + search goal (curr `div` 2)
