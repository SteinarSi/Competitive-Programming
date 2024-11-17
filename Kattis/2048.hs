import           Control.Arrow            ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.List                (transpose)
import           Data.Maybe               (fromJust)

main :: IO ()
main = do
    (grid, (before, after)) <- C.getContents <&> (
                C.lines
            >>> map (C.words >>> map readInt)
            >>> take 4 
                    &&& 
                (last >>> head >>> parseTransform)
        )
    before grid
        & map ((filter (/=0)) >>> fall >>> pad)
        & after
        & map (map (show >>> C.pack) >>> C.unwords)
        & C.unlines
        & C.putStr

parseTransform :: Int -> ([[a]] -> [[a]], [[a]] -> [[a]])
parseTransform 0 = (id, id)
parseTransform 1 = (transpose, transpose)
parseTransform 2 = (map reverse, map reverse)
parseTransform 3 = (transpose >>> map reverse, map reverse >>> transpose)

fall :: [Int] -> [Int]
fall [] = []
fall [x] = [x]
fall (x:y:xs) | x == y    = 2*x : fall xs
              | otherwise =   x : fall (y:xs)

pad :: [Int] -> [Int]
pad xs = xs ++ replicate (4 - length xs) 0

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
