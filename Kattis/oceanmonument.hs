import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> (\[g,e] -> dp ! (g,e)) >>> show)
        >>> unlines
        >>> putStr
    )

dp :: Array (Int,Int) Int
dp = listArray rng (map f (range rng))
  where
    rng = ((0,0),(40,20))

    f (0,0) = 1
    f (_,0) = 1
    f (g,e)
        | g < 2     = dp ! (g+1,e-1)
        | otherwise = dp ! (g-1,e) + dp ! (g+1,e-1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
