import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\(n:x:y:w:h:_) -> solve n x y w h))
        >>> C.intercalate (C.pack "\n")
        >>> C.putStrLn
    )

solve :: Int -> Int -> Int -> Int -> Int -> C.ByteString
solve n x y w h = map (\y' -> map (hadamard n y' >>> show >>> C.pack) [x..x+w-1] & C.unwords) [y..y+h-1] & C.unlines

hadamard :: Int -> Int -> Int -> Int
hadamard 1 _ _ = 1
hadamard n x y = scale * hadamard half x' y'
    where
        half = n `div` 2
        (x', y', scale) = case (x >= half, y >= half) of
            (False, False) -> (x     , y     ,  1)
            (False, True ) -> (x     , y-half,  1)
            (True , False) -> (x-half, y     ,  1)
            (True , True ) -> (x-half, y-half, -1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
