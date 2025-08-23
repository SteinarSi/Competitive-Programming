import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array.Base       (newArray, writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import           Data.Bits
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (readInt >>> solve >>> show >>> C.pack)
        >>> C.unwords
        >>> C.putStrLn
    )

solve :: Int -> Int
solve = (arr !)
    where arr = runSTUArray $ do
            arr <- newArray (0,255) 0
            forM_ [0..255] (\x -> writeArray arr (encode x) x)
            pure arr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

encode :: Int -> Int
encode x = (x `xor` (x `shift` 1)) .&. (1 `shift` 8 - 1)
