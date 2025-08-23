import           Data.IntSet (IntSet, difference, elems, fromList)

main :: IO ()
main = do
    r:_:xs <- fmap (map read . words) getContents
    putStrLn $ case elems (difference (fromList [1..r]) (fromList xs)) of
        []  -> "too late"
        x:_ -> show x
