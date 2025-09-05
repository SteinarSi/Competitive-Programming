import           Control.Arrow ((>>>))
import           Data.Char     (isDigit)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    x <- readTime
    y <- readTime
    print $ if y < x
        then 24*60*60 + y - x
        else y - x

readTime :: IO Int
readTime = do
    [h,m,s] <- getLine <&> (words >>> filter (head >>> isDigit) >>> map read)
    pure (s + 60*m + 60*60*h)
