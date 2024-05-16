import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (
                C.words
            >>> map readInt
            >>> (\(a:b:c:_) -> check a b c)
            >>> bool "NO" "YES"
            >>> C.pack
            >>> C.putStrLn
        )
    )

check :: Int -> Int -> Int -> Bool
check a b c = let d = b^2 - 4*a*c
              in  d >= 0 && isRound (sqrt (fromIntegral d))

isRound :: Double -> Bool
isRound x = abs (fromIntegral (round x) - x) <= 0.0000001

squares :: [Int]
squares = map (^2) [0..]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
