import           Control.Monad (forM_)
import qualified Data.IntSet   as S

main :: IO ()
main = do
    n <- fmap read getLine
    forM_ [1..n] $ \i -> do
        getLine
        oddManOut <- fmap (head . S.elems . foldr insertOrRemove S.empty . map read . words) getLine
        putStrLn ("Case #" ++ show i ++ ": " ++ show oddManOut)

insertOrRemove :: Int -> S.IntSet -> S.IntSet
insertOrRemove x seen | S.member x seen = S.delete x seen
                      | otherwise = S.insert x seen
