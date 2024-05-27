import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >> getLine >>= (check 0 "" >>> putStrLn)

check :: Int -> String -> String -> String
check _ _ "" = "ok so far"
check i "" (')':_) = ") " <> show i
check i "" (']':_) = "] " <> show i
check i "" ('}':_) = "} " <> show i
check i (y:ys) (')':xs) | y /= '(' = ") " <> show i
                        | otherwise = check (i+1) ys xs
check i (y:ys) (']':xs) | y /= '[' = "] " <> show i
                        | otherwise = check (i+1) ys xs
check i (y:ys) ('}':xs) | y /= '{' = "} " <> show i
                        | otherwise = check (i+1) ys xs
check i ys (' ':xs) = check (i+1) ys xs
check i ys (x:xs) = check (i+1) (x:ys) xs
