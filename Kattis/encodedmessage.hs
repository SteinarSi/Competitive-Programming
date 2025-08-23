import           Control.Monad (replicateM_)
import           Data.List     (transpose)

main :: IO ()
main = do
    t <- fmap read getLine
    replicateM_ t $ do
        msg <- getLine
        let side   = round $ sqrt $ fromIntegral (length msg)
            square = chunksOf side msg
            secret = concat(reverse (transpose square))
        putStrLn secret

chunksOf :: Int -> String -> [String]
chunksOf _ "" = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
