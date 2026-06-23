import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (nub)

main :: IO ()
main  = do
    h1:w1:xs <- getContents <&> words

    let valid = read h1 == length xs
            && all (length >>> (== read w1)) xs
            && any (elem '-') xs
            && all (notElem '-') trd
            && all (s==) snd
            && notElem '-' ms
        (s:snd,trd) = span (elem '-') (dropWhile (notElem '-') xs)
        (ps,ms) = span (=='-') (dropWhile ('+' ==) s)
        h2 = 1 + length snd
        w2 = length ps

    putStrLn $ if valid
        then unwords ([h1, w1, show h2, show w2])
        else "Neibb"
