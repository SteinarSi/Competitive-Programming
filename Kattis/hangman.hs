import           Control.Applicative (Applicative (liftA2))

main :: IO ()
main = liftA2 (hangman 10) getLine getLine >>= putStrLn

hangman :: Int -> String -> String -> String
hangman 0 _ _ = "LOSE"
hangman _ "" _ = "WIN"
hangman k secret (x:xs) | elem x secret = hangman k (filter (/=x) secret) xs
                        | otherwise     = hangman (k-1) secret xs
