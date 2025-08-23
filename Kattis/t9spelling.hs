import           Control.Monad (forM_)
import           Data.List     (elemIndex, findIndex)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    n <- fmap read getLine
    forM_ [1..n] $ \i -> do
        message <- getLine
        putStrLn ("Case #" ++ show i ++ ": " ++ combine (map convert message))

combine :: [String] -> String
combine [] = []
combine [x] = x
combine ((x:xs):(y:ys):zs) | x == y = (x:xs) ++ " " ++ combine ((y:ys):zs)
                           | otherwise = (x:xs) ++ combine ((y:ys):zs)

convert :: Char -> String
convert c = concat $ replicate presses (show button)
    where
        presses = 1 + fromJust (elemIndex c (keys !! button))
        button = fromJust (findIndex (elem c) keys)

keys :: [String]
keys = [
        " ",
        "",
        "abc",
        "def",
        "ghi",
        "jkl",
        "mno",
        "pqrs",
        "tuv",
        "wxyz"
    ]
