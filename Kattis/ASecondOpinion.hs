import           Data.Functor ((<&>))

main :: IO ()
main = do
    s <- getContents <&> read
    let (m, seconds) = quotRem s 60
        (hours, minutes) = quotRem m 60
    putStrLn (show hours <> " : " <> show minutes <> " : " <> show seconds)
