import           Control.Arrow ((&&&), (>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> head &&& last)
        >>> solve
        >>> print
    )

solve :: [(Double,Double)] -> Double
solve xs = minimum $ map ((*(2*pi)) >>> (/36000) >>> (`rotate` xs) >>> height) [0..36000]

height :: [(Double,Double)] -> Double
height = map snd >>> maximum &&& minimum >>> uncurry (-)

rotate :: Double -> [(Double,Double)] -> [(Double,Double)]
rotate theta = map (\(x,y) -> (cos theta * x - sin theta * y, sin theta * x + cos theta * y))
