import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (minimumBy)
import           Data.Maybe            (mapMaybe)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.split '=' >>> \[x,y] -> solulu x y)
        >>> unlines
        >>> putStr
    )

solulu :: C.ByteString -> C.ByteString -> String
solulu text msg = [0..n-1]
    & filter (C.index text >>> (== C.index msg 0))
    & mapMaybe (\s -> (s,) <$> end s 0)
    & minimumBy (compare `on` (\(s,t) -> (t-s,s)))
    & uncurry (printf "%d %d")
  where
    end :: Int -> Int -> Maybe Int
    end i j
        | i >= n                          = Nothing
        | C.index text i /= C.index msg j = end (i+1) j
        | j+1 == m                        = Just i
        | otherwise                       = end (i+1) (j+1)

    n = C.length text
    m = C.length msg
