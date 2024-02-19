import           Data.Char (isDigit)

main :: IO ()
main = do
    [kind,word] <- fmap words getLine
    putStrLn $ case kind of
        "E" -> encode word
        "D" -> decode word

decode :: String -> String
decode "" = ""
decode (x:xs) = replicate (read a) x ++ decode b
    where (a,b) = span isDigit xs

encode :: String -> String
encode ""     = ""
encode xs@(x:_) = x : show (length a) ++ encode b
    where (a,b) = span (x==) xs
