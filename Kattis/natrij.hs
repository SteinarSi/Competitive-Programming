import           Control.Arrow (second, (>>>))
import           Data.Function ((&))
import           Data.List     (intercalate)
import           Text.Printf   (printf)

main :: IO ()
main = do
    x <- getTimestamp
    y <- getTimestamp

    let diff = case compare y x of
            GT -> y - x
            EQ -> 60*60*24
            LT -> y + 60*60*24 - x

    putStrLn $ showTimestamp diff

showTimestamp :: Int -> String
showTimestamp x = [h, m, s]
        & map (printf "%02d")
        & intercalate ":"
    where
        (h, (m, s)) = quotRem x (60*60)
            & second (`quotRem` 60)

getTimestamp :: IO Int
getTimestamp = do
    [h1,h2,_,m1,m2,_,s1,s2] <- getLine
    pure (60*60*read [h1,h2] + 60*read[m1,m2] + read [s1,s2])
