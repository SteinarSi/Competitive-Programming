import           Data.List (elemIndices, findIndex, isPrefixOf, tails)

main :: IO ()
main = do
    xs <- getLine

    let
        format :: Int -> String -> String
        format i err = xs <> "\n" <> replicate i ' ' <> "^--" <> err

    putStrLn $ case elemIndices '@' xs of
        []                                    -> "@ symbol is missing."
        _:a:_                                 -> format a "there is an extra @ symbol here."
        [0]                                   -> "There is nothing before the @ symbol."
        [a] | a == length xs - 1              -> format (length xs) "there is nothing after the @ symbol."
            | otherwise                       -> case elemIndices '.' xs of
                0:_                           -> "Email address starts with a dot."
                ds | elem (a-1) ds            -> format (a-1) "there is an extra dot here."
                   | otherwise                -> case findIndex (".." `isPrefixOf`) (tails xs) of
                        Just i                -> format i "there are consecutive dots here."
                        Nothing | all (<a) ds -> "Top-level-domain is missing."
                                | otherwise   -> "All good."
