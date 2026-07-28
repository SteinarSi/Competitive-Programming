import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getLine
    putStrLn $ if xs == C.reverse xs
        then "Palindrome!"
        else "Nothing special about this string :("
