import Data.Bool (bool)

main :: IO ()
main = do
    v:_:xs <- fmap lines getContents
    mapM_ ((\[name, limit] -> putStrLn $ name ++ " " ++ bool "lokud" "opin" (read v <= read limit+0)) . words) xs
