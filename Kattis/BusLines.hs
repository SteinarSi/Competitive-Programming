import           Control.Arrow ((>>>))
import           Data.Function ((&))

main :: IO ()
main = do
    [n,m] <- fmap (words >>> map read) getLine
    let lines = [(a,a+1) | a <- [1..n-1]] ++ [(a,a+2) | a <- [1..n-2]]
    if m < n-1 || m > length lines
        then putStrLn "-1"
        else mapM_ (\(a,b) -> putStrLn (show a ++ " " ++ show b)) (take m lines)
