import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> chunksOf 5
        >>> map (\(goal:curr:xs) -> solve goal curr xs)
        >>> mapM_ print
    )

solve :: C.ByteString -> C.ByteString -> [C.ByteString] -> Int
solve goal curr xs = map (pickAndBackspace >>> succ) xs
        & (pickAndBackspace curr :)
        & minimum

    where
        pickAndBackspace :: C.ByteString -> Int
        pickAndBackspace x = C.length x + C.length goal - 2 * sharedPrefix x

        sharedPrefix :: C.ByteString -> Int
        sharedPrefix x = C.zipWith (==) goal x
            & takeWhile id
            & length

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
