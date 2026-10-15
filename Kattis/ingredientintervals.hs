import           Control.Arrow ((>>>))
import           Data.Maybe    (catMaybes, fromMaybe, mapMaybe)
import           Text.Printf   (printf)

epsilon :: Double
epsilon = 1e-7

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map parse
        >>> bounds
        >>> minimums
        >>> maximums
        >>> map format
        >>> unlines
        >>> putStr
    )

bounds :: [(String, Maybe Double)] -> (Double,[(String,Double,Double)])
bounds xs = (used, catMaybes (zipWith3 (\l h (x,p) -> maybe (Just (x,l,h)) (const Nothing) p) los his xs))
  where
    his = scanl (\r -> snd >>> fromMaybe r) 100 xs
    los = reverse (scanl (\r -> snd >>> fromMaybe r) 0 (reverse xs))
    used = sum (mapMaybe snd xs)

viable :: (String,Double) -> Double -> Double -> [(String,Double,Double)] -> Maybe Bool
viable (x,p) _ _ [] = error "bruh"
viable (x,p) rem unused ((y,lo,hi):xs)
    | x == y && rem >= p-lo = viable' (rem-(p-lo)-unused) xs
    | x == y = Just False
    | lo >= p = viable (x,p) rem (unused+(hi-lo)) xs
    | rem >= p-lo = viable (x,p) (rem-(p-lo)) (unused+(hi-p)) xs
    | otherwise = Just False
  where
    viable' :: Double -> [(String,Double,Double)] -> Maybe Bool
    viable' rem _ | rem <= 0 = Nothing
    viable' rem [] = Just True
    viable' rem ((y,lo,hi):xs) = viable' (rem-(min hi p - lo)) xs

minimums :: (Double, [(String,Double,Double)]) -> (Double,[(String,Double,Double)])
minimums (used,xs) = (used, map (\(x,lo,hi) -> (x,bin x lo hi,hi)) xs)
  where
    bin :: String -> Double -> Double -> Double
    bin x lo hi
        | hi-lo < epsilon = mi
        | otherwise = case viable (x,mi) rem 0 xs of
            Just True -> bin x mi hi
            _         -> bin x lo mi
      where
        mi = (lo+hi) / 2
    rem = 100 - used - sum (map (\(_,lo,_) -> lo) xs)

maximums :: (Double, [(String,Double,Double)]) -> [(String,Double,Double)]
maximums (used,xs) = map (\(x,lo,hi) -> (x, lo, bin x lo hi)) xs
  where
    bin :: String -> Double -> Double -> Double
    bin x lo hi
        | hi-lo < epsilon = mi
        | otherwise = case viable (x,mi) rem 0 xs of
            Just False -> bin x lo mi
            _          -> bin x mi hi
      where
        mi = (lo+hi) / 2
    rem = 100 - used - sum (map (\(_,lo,_) -> lo) xs)

format :: (String,Double,Double) -> String
format (x,lo,hi) = printf "%s %.6f %.6f" x lo hi

parse :: String -> (String, Maybe Double)
parse xs = case words xs of
    [x]   -> (x, Nothing)
    [x,p] -> (x, Just (read p))
