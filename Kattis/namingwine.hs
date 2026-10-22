import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> init >>> read)
        >>> solve (M.fromList [(1,"standard"),(12,"balthazar"),(15,"nebuchadnezzar")]) [x:xs | xs <- "" : names, x <- ['a'..'z']]
        >>> unlines
        >>> putStr
    )

solve :: M.Map Double String -> [String] -> [Double] -> [String]
solve _ _ [] = []
solve sizes (n:ns) (x:xs) = case sizes M.!? x of
    Nothing -> n : solve (M.insert x n sizes) ns xs
    Just  y -> y : solve sizes (n:ns) xs

names :: [String]
names = [x:xs | xs <- "" : names, x <- ['a'..'z']]
