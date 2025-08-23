import           Control.Arrow ((>>>))
import           Data.List     (sort)
import           Data.Map      (Map, assocs, empty, insertWith)
import           Data.Set      (Set, singleton, size, union)

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> foldr ((\[first, second, course] -> insertWith union course (singleton (first, second))) . words) empty
        >>> assocs
        >>> map (fmap (size >>> show))
        >>> sort
        >>> mapM_ (\(name, count) -> putStrLn (name ++ " " ++ count))
    )
