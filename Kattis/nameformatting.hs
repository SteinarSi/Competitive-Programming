import           Data.Char (toUpper)

main :: IO ()
main = do
    [x:xs, y:_] <- words <$> getLine
    putStrLn (toUpper y : ". " <> [toUpper x] <> init xs)
