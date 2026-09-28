import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getLine >>= (
            C.unpack
        >>> count (0,0,0,0,0)
        >>> batch 5
        >>> unlines
        >>> putStr
    )

batch :: Int -> [(Char,Int)] -> [String]
batch 0 _  = []
batch i xs
    | null ms       = "0" : batch (i-1) xs
    | length ms < i = "0" : batch (i-1) xs
    | otherwise     = show m <> " " <> map fst (filter (snd >>> (>=m)) xs) : batch (i-1) (map (fmap (subtract m)) xs)
  where
    ms = filter (>0) (map snd xs)
    m = minimum ms

count :: (Int,Int,Int,Int,Int) -> String -> [(Char,Int)]
count (r,o,y,g,p) "" = [('R',r),('O',o),('Y',y),('G',g),('P',p)]
count (r,o,y,g,p) (x:xs) = count (case x of
        'R' -> (r+1,o,y,g,p)
        'O' -> (r,o+1,y,g,p)
        'Y' -> (r,o,y+1,g,p)
        'G' -> (r,o,y,g+1,p)
        'P' -> (r,o,y,g,p+1))
    xs
