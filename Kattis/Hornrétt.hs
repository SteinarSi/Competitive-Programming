import Control.Arrow ((>>>))
import Data.Functor  ((<&>))
import Data.Maybe    (catMaybes, listToMaybe)
import Data.Function ((&))

main :: IO ()
main = do
    [a,b,c] <- getContents <&> (words >>> map read)

    [area a b c, area b a c, area c a b] 
        & catMaybes
        & listToMaybe
        & maybe "-1" show
        & putStrLn

area :: Int -> Int -> Int -> Maybe Int
area a b c | a^2 + b^2 == c^2 = Just ((a*b) `div` 2)
           | otherwise        = Nothing
