import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs <- getLine <&> words
    putStrLn $ case xs of
        [x] -> x
        _   -> last xs <> ", " <> unwords (map (take 1 >>> (<>".")) (init xs))
