import           Control.Monad      (replicateM)
import           Data.Array.Unboxed (UArray, array, (!))

main :: IO ()
main = do
    [n, m, q] <- fmap (map read . words) getLine
    answers <- replicateM n getLine
    questions <- fmap (map ((\(a:b:_) -> (read a, head b)) . words) . lines) getContents
    let questionnaire = array ((1, 1), (n, m)) [ ((y, x), a) | (y, answer) <- zip [1..] answers, (x, a) <- zip [1..] answer ] :: UArray (Int,Int) Char

    putStrLn $ guessWho questionnaire [1..n] questions

guessWho :: UArray (Int,Int) Char -> [Int] -> [(Int, Char)] -> String
guessWho _ [] _ = error "bruh"
guessWho _ [x] _ = "unique\n" ++ show x
guessWho _ people [] = "ambiguous\n" ++ show (length people)
guessWho questionnaire people ((q,a):xs) = guessWho questionnaire (filter (\p -> questionnaire ! (p, q) == a) people) xs
