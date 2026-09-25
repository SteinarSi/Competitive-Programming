import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> property1)
        >>> property3
        >>> putStrLn
    )

property1 :: String -> (Int,Int,Int)
property1 ""       = (0,0,0)
property1 ('R':xs) = property2 (1,1,0) xs
property1 xs       = property2 (0,0,0) xs

property2 :: (Int,Int,Int) -> String -> (Int,Int,Int)
property2 (s,b,r) ""           = (s,b,r)
property2 (s,b,r) ('R':'R':xs) = property2 (s+1,b+1,r+1) xs
property2 (s,b,r) ('B':xs)     = property2 (s,b+1,r) xs
property2 (s,b,r) ('R':xs)     = property2 (s,b,r+1) xs

property3 :: [(Int,Int,Int)] -> String
property3 xs = maybe "impossible" (sum >>> show) (mapM (\(s,b,r) -> bool Nothing (Just (s+bs-b)) (bs-b<=r)) xs)
  where
    bs = maximum (map (\(_,b,_) -> b) xs)
