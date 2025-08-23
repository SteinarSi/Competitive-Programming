import           Data.List (isInfixOf)

main :: IO ()
main = getLine >>= putStrLn . classify

classify :: String -> String
classify s = case (":)" `isInfixOf` s, ":(" `isInfixOf` s) of
    (True,  False) -> "alive"
    (True,  True ) -> "double agent"
    (False, True ) -> "undead"
    (False, False) -> "machine"
