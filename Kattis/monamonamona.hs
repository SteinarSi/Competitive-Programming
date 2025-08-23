main :: IO ()
main = do
    name:_ <- words <$> getLine
    putStrLn (name <> " " <> name <> " " <> name)
