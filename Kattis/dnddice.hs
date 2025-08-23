import           Control.Arrow      ((>>>))
import qualified Data.IntMap.Strict as M
import           Data.List          (sortOn)

main :: IO ()
main = getContents >>= (
            words
        >>> map read
        >>> zipWith (flip replicate) [4,6,8,12,20]
        >>> concat
        >>> rolls
        >>> M.toList
        >>> sortOn (snd >>> negate)
        >>> map (fst >>> show)
        >>> unwords
        >>> putStrLn
    )

rolls :: [Int] -> M.IntMap Integer
rolls []     = M.singleton 0 1
rolls (x:xs) = M.fromListWith (+) $ do
    (t,c) <- M.toAscList (rolls xs)
    r <- [1..x]
    pure (t+r,c)
