import           Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 20 (putStrLn "Hipp hipp hurra!")
