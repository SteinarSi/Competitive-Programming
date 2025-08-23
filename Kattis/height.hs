import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import           Data.Bifunctor        (Bifunctor (bimap), first)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> map readInt
            >>> (\(c:xs) -> [c, fst (order xs)])
            >>> map (show >>> C.pack)
            >>> C.unwords
            >>> C.putStrLn
        )
    )

order :: Ord a => [a] -> (Int, [a])
order = foldr (\a (c, xs) -> first (c+) (insert a xs)) (0, [])

insert :: Ord a => a -> [a] -> (Int, [a])
insert a [] = (0, [a])
insert a (x:xs) | x > a = (0, a:x:xs)
                | otherwise = bimap succ (x:) (insert a xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
