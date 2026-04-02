import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (last >>> parse))
        >>> track M.empty
        >>> unlines
        >>> putStr
    )

track :: M.Map C.ByteString Int -> [(C.ByteString,Int)] -> [String]
track total []         = []
track total ((x,y):xs) = M.insertLookupWithKey (const (+)) x y total
    & (maybe y (y+) >>> format x) *** flip track xs
    & uncurry (:)

format :: C.ByteString -> Int -> String
format c x = printf "%s $%d.%.2d" (C.unpack c) q r
  where
    (q,r) = quotRem x 100

parse :: C.ByteString -> Int
parse x = 100 * dec + flt
  where
    Just (dec,rest) = C.readInt (C.drop 1 x)
    Just (flt,_)    = C.readInt (C.drop 1 rest)
