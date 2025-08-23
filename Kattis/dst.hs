import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

data FB = F | B deriving (Read, Eq)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> (\[fb,d,h,m] -> (read (C.unpack fb), readInt d, readInt h, readInt m))
            >>> solve
            >>> C.putStrLn
        )
    )

solve :: (FB, Int, Int, Int) -> C.ByteString
solve (fb, d, h, m) = C.unwords $ map (show >>> C.pack) [h', m']
    where minutes = (h * 60 + m + delta) `mod` (24*60)
          delta | fb == B   = -d
                | otherwise = d
          (h', m') = quotRem minutes 60

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>>  fst
