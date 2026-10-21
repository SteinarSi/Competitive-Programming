import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readNum >>> goldgoldgold >>> show)
        >>> unlines
        >>> putStr
    )

goldgoldgold :: [Double] -> Int
goldgoldgold [ng,dg,nt,dt] = max 1 (ceiling (log (1 - nt / dt) / log (1 - ng / dg)))

readNum :: Num n => C.ByteString -> n
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
