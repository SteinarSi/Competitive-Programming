import           Control.Arrow         (second, (>>>))
import           Data.Array.Unboxed    (UArray, inRange, indices, listArray,
                                        (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> chunksOf 9
        >>> map (drop 1
            >>> concatMap C.unpack
            >>> listArray rng
            >>> check
            >>> bool "NO" "YES")
        >>> unlines
        >>> putStr
    )

rng :: ((Int, Int), (Int, Int))
rng = ((1,1),(8,8))

check :: UArray (Int,Int) Char -> Bool
check board = any threats (indices board)
  where
    threats :: (Int,Int) -> Bool
    threats (y,x) = case board ! (y,x) of
        'p' -> any (\v -> inRange rng v && board ! v == 'K') [(y+1,x+1),(y+1,x-1)]
        'n' -> any (\v -> inRange rng v && board ! v == 'K') [(y-1,x-2),(y-2,x-1),(y-2,x+1),(y-1,x+2),(y+1,x+2),(y+2,x+1),(y+2,x-1),(y+1,x-2)]
        'b' -> any (\(dy,dx) -> run (dy,dx) (y+dy,x+dx)) [(-1,-1),(-1,1),(1,1),(1,-1)]
        'r' -> any (\(dy,dx) -> run (dy,dx) (y+dy,x+dx)) [(-1,0),(0,1),(1,0),(0,-1)]
        'q' -> any (\(dy,dx) -> run (dy,dx) (y+dy,x+dx)) [(-1,-1),(-1,1),(1,1),(1,-1),(-1,0),(0,1),(1,0),(0,-1)]
        _   -> False
      where
        run :: (Int,Int) -> (Int,Int) -> Bool
        run (dy,dx) (y',x') = inRange rng (y',x') && (board ! (y',x') == 'K' || board ! (y',x') == '.' && run (dy,dx) (y'+dy,x'+dx))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
