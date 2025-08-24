import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Maybe    (listToMaybe, mapMaybe)

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map (read >>> (^2)))

    [a+b,a-b,b-a]
        & mapMaybe root
        & listToMaybe
        & maybe "Pythagoras is sad :(" show
        & putStrLn

root :: Int -> Maybe Int
root x
    | x <= 0    = Nothing
    | sx^2 == x = Just sx
    | otherwise = Nothing
  where
    sx = round (sqrt (fromIntegral x))
