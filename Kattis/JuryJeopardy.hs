import           Control.Arrow         (second, (>>>))
import           Control.Monad         (forM_)
import           Data.Array.Base       (elems, newArray, writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Tuple            (swap)

main :: IO ()
main = C.getLine >>= C.putStrLn >> C.getContents >>= (
            C.lines
        >>> mapM_ solve
    )

solve :: C.ByteString -> IO ()
solve input = do
    C.putStrLn header
    elems grid
        & chunksOf n
        & map (map (bool '#' '.') >>> C.pack)
        & C.unlines
        & C.putStr

    where
        path = travel input
        (minX,minY,maxX,maxY) = borders (0,0,0) path
        header = C.unwords (map (show >>> C.pack) [m, n])
        m = maxY - minY + 1
        n = maxX - minX + 1
        grid = runSTUArray $ do
            arr <- newArray ((minY,minX),(maxY,maxX)) False
            forM_ path (swap >>> flip (writeArray arr) True)
            pure arr

borders :: (Int,Int,Int) -> [(Int,Int)] -> (Int,Int,Int,Int)
borders (minY,maxX,maxY) [] = (0, minY-1, maxX+1, maxY+1)
borders (minY,maxX,maxY) ((x,y):xs) = borders (min y minY, max x maxX, max y maxY) xs

travel :: C.ByteString -> [(Int,Int)]
travel = C.foldl' (\(p:ps) c -> move p c : (p:ps)) [(1,(0,0))] >>> map snd

move :: (Int,(Int,Int)) -> Char -> (Int,(Int,Int))
move (dir,(x,y)) c = (dir',pos)
    where
        pos  = (x + dir2X dir', y + dir2Y dir')
        dir' = case c of
            'L' -> pred dir & (`mod` 4)
            'R' -> succ dir & (`mod` 4)
            'B' -> succ dir & succ & (`mod` 4)
            _   -> dir

        dir2X :: Int -> Int
        dir2X dir = - (dir `mod` 2) * (dir - 2)

        dir2Y :: Int -> Int
        dir2Y dir = ((dir+1) `mod` 2) * (dir - 1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
