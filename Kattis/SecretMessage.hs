import           Control.Monad (replicateM_)
import           Data.List     (find, transpose)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM_ n $ do
        msg <- getLine
        let l = length msg
            k = fromJust (find ((>=l) . (^2)) [1..])
            m = k^2
            ls = chunksOf k (msg ++ replicate (m-l) '*')
            secret = concatMap (reverse . filter (/='*')) (transpose ls)
        putStrLn secret

chunksOf :: Int -> String -> [String]
chunksOf _ "" = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
