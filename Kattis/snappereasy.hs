import           Control.Arrow         ((>>>))
import           Data.Bits             (Bits (shiftL, xor, (.&.)))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> tail
        >>> map (C.words
            >>> map readInt
            >>> (\(a:b:_) -> (a,b))
            )
        >>> zip [1..]
        >>> mapM_ ((\(i, (a,b)) -> "Case #" <> show i <> ": " <> solve a b)
            >>> C.pack
            >>> C.putStrLn
            )
    )

solve :: Int -> Int -> String
solve n k | xor goal (goal .&. k) == 0 = "ON"
          | otherwise = "OFF"
    where
        goal :: Int
        goal = shiftL 1 n - 1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
