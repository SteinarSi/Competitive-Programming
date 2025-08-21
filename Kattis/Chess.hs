import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, digitToInt, ord)
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (parse >>> uncurry solve >>> format)
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
solve u@(c1,r1) v@(c2,r2) | u == v = Just [v]
                          | diff1 == 0 || diff2 == 0 = Just [u,v]
                          | odd (dia1 u) /= odd (dia1 v) = Nothing
                          | valid p1 = (u:) <$> solve p1 v
                          | valid p2 = (u:) <$> solve p2 v
                          | otherwise = Nothing
  where
    diff1 = dia1 v - dia1 u
    diff2 = dia2 v - dia2 u
    dia1 (c,r) = c+r
    dia2 (c,r) = 9-c+r
    p1 = (c1 + diff1 `div` 2, r1 + diff1 `div` 2)
    p2 = (c1 - diff2 `div` 2, r1 + diff2 `div` 2)

valid :: (Int,Int) -> Bool
valid = inRange ((1,1),(8,8))

format :: Maybe [(Int,Int)] -> String
format Nothing   = "Impossible"
format (Just xs) = unwords (show (length xs - 1) : concatMap (\(c,r) -> [[chr (c + ord 'A' - 1)], show r]) xs)

parse :: C.ByteString -> ((Int,Int),(Int,Int))
parse x = (
    (ord c1 - ord 'A' + 1, digitToInt r1),
    (ord c2 - ord 'A' + 1, digitToInt r2)
    )
  where
    [[c1],[r1],[c2],[r2]] = words (C.unpack x)
