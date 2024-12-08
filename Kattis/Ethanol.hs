import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getContents <&> read

    putStr $ unlines $ [
            ' ' : concat (replicate n " H"),
            ' ' : concat (replicate n " |"),
            "H-" <> concat (replicate n "C-") <> "OH",
            ' ' : concat (replicate n " |"),
            ' ' : concat (replicate n " H")
        ]
