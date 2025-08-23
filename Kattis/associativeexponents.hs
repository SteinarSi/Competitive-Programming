import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b,c] <- getLine <&> (words >>> map read)
    putStrLn $ if a==1 || a /= 1 && c==1 || a>1 && b==2 && c==2
        then "What an excellent example!"
        else "Oh look, a squirrel!"
