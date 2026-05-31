import           Data.Char    (digitToInt)
import           Data.Functor ((<&>))
import           Data.List    (elemIndex)

main :: IO ()
main = do
    [s,c] <- getLine <&> words
    let Just i = elemIndex s (iterate decrypt c)
    print i

decrypt :: String -> String
decrypt []       = []
decrypt (c:x:xs) = replicate (digitToInt c) x <> decrypt xs
