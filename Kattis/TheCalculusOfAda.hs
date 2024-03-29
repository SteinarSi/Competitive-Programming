import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.words
        >>> tail
        >>> map readInt
        >>> ada 0 0
        >>> C.putStrLn
    )

ada :: Int -> Int -> [Int] -> C.ByteString
ada d s [] = error "bruh"
ada d s (x:xs) | all (x==) xs = C.unwords $ map (show >>> C.pack) [d, s+x]
               | otherwise    = ada (d+1) (s+last xs) (zipWith (-) xs (x:xs))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
