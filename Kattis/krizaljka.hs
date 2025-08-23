import           Control.Monad (forM_, guard)
import           Data.List     (elemIndex)

main :: IO ()
main = do
    [xs,ys] <- fmap words getLine
    let (i,j) = head $ do
            (i,x) <- zip [0..] xs
            guard (x `elem` ys)
            let Just j = elemIndex x ys
            pure (i, j)
    forM_ (zip [0..] ys) $ \(j',x) -> putStrLn $ if j' == j
        then xs
        else replicate i '.' ++ [x] ++ replicate (length xs - i - 1) '.'
