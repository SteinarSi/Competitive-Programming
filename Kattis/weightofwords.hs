import           Control.Arrow ((>>>))
import           Data.Char     (chr, ord)
import           Data.Functor  ((<&>))
import           Data.Maybe    (fromMaybe)

main :: IO ()
main = do
    [l, w] <- getLine <&> (words >>> map read)
    putStrLn (fromMaybe "impossible" (solve l w))

solve :: Int -> Int -> Maybe String
solve l w | w < l = Nothing
          | w > l*26 = Nothing
          | w == l = Just (replicate w 'a')
          | w - 26 >= l - 1 = fmap ('z':) (solve (l-1) (w-26))
          | otherwise = fmap (chr (w - l + ord 'a'):) (solve (l-1) (l-1))
