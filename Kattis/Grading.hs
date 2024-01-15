import           Data.List  (find)
import           Data.Maybe (fromMaybe)

main :: IO ()
main = do
    grades <- fmap (zip "ABCDE" . map read . words) getLine
    score <- fmap read getLine :: IO Int

    putStrLn . pure $ case find ((score >=) . snd) grades of
        Nothing     -> 'F'
        Just (g, _) -> g
