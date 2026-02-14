import           Control.Arrow ((>>>))
import           Data.Char     (isAlphaNum)

main :: IO ()
main = interact (read >>> (`take` passwords) >>> unlines)

special :: [Char]
special = filter (isAlphaNum >>> not) ['!'..'~']

passwords :: [String]
passwords = do
    (l,u) <- zip ['a'..'z'] ['A'..'Z']
    (d1,d2) <- zip ['0'..'1'] ['0'..'1']
    (s1,s2) <- zip special special
    (s3,s4) <- zip ['!'..'~'] ['!'..'~']
    pure [l,u,d1,d2,s1,s2,s3,s4]
