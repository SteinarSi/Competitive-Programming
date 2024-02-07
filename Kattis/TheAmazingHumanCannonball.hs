import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> map (words >>> map read >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [Double] -> String
solve [v0, θ', x1, h1, h2] | h1 + 1 <= y && y <= h2 - 1 = "Safe"
                           | otherwise = "Not Safe"
    where
        θ = θ' * pi / 180
        x t = v0 * t * cos θ
        t = x1 / (v0 * cos θ)
        y = max 0 (v0 * t * sin θ - g*t*t/2)

g :: Double
g = 9.81
