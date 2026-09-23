import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    nc:xs <- C.lines <$> C.getContents
    let (h,(m,s)) = xs
            & map (C.split ':' >>> map readInt >>> \[mm,ss] -> 60 * mm + ss)
            & crossfade (readInt (last (C.words nc)))
            & (`quotRem` 3600)
            & second (`quotRem` 60)

    printf "%.2d:%.2d:%.2d\n" h m s

crossfade :: Int -> [Int] -> Int
crossfade _ []       = 0
crossfade _ [x]      = x
crossfade c (x:y:xs) = x-c + crossfade c (y:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
