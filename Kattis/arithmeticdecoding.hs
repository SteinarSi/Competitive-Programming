import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n',d',f'] <- C.getContents <&> C.words
    let n = readInt n'
        x = readBinaryDouble f'
        p = fromIntegral (readInt d') / 8

    decode p x n 0 1
        & C.pack
        & C.putStrLn

readBinaryDouble :: C.ByteString -> Double
readBinaryDouble = C.drop 2
        >>> C.unpack
        >>> map (digitToInt >>> fromIntegral)
        >>> zipWith (*) (map (2^^) [-1,-2..])
        >>> sum

decode :: Double -> Double -> Int -> Double -> Double -> String
decode _ _ 0 _ _ = ""
decode p x n a b | x < c     = 'A' : decode p x (n-1) a c
                 | otherwise = 'B' : decode p x (n-1) c b
    where c = a + p * (b - a)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
