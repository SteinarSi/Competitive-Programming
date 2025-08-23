import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    (s,answer):xs <- C.getContents <&> (C.lines >>> parse)
    map (score (readNum s) answer) xs
        & C.concat
        & C.putStr

score :: Double -> C.ByteString -> (C.ByteString,C.ByteString) -> C.ByteString
score s answer (name,attempt) = C.pack (printf "%s: %1.1f\n" (C.unpack name) ans)
    where 
        right = C.zipWith (==) answer attempt
            & filter id
            & length
            & fromIntegral
            & (/s)
            & (*10)
        base = fromIntegral (floor right)
        rest = right - base
        ans | rest >= 0.75 = base + 1
            | rest >= 0.25 = base + 0.5
            | otherwise    = base

parse :: [C.ByteString] -> [(C.ByteString, C.ByteString)]
parse [] = []
parse (x:y:xs) = (x, C.filter isAlpha y) : parse xs

readNum :: Num n => C.ByteString -> n
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
