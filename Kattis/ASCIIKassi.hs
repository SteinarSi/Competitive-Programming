import           Control.Monad (replicateM_)

main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn ('+' : replicate n '-' ++ "+")
    replicateM_ n $ putStrLn ('|' : replicate n ' ' ++ "|")
    putStrLn ('+' : replicate n '-' ++ "+")
