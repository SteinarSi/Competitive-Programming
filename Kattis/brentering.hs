import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.List     (find)

main :: IO ()
main = do
    xs <- getLine
    let Just (i,_) = zip [1..] xs
            & reverse
            & find (snd >>> (`elem` "aeiou"))
    putStrLn (take i xs <> "ntry")
