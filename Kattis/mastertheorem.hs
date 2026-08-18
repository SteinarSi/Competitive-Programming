import           Control.Arrow ((>>>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    xs <- words <$> getLine
    let [a,b,c,d] = map read (init xs)
        k = read (last xs)
        logba = logBase b a
    putStrLn $ unwords $ filter (null >>> not) $ case compare d logba of
        LT -> [formatNX logba]
        GT -> [formatNX d, formatLogK k]
        EQ -> case compare k (-1) of
            GT -> [formatNX logba, formatLogK (k+1)]
            EQ -> [formatNX logba, "log log n"]
            LT -> [formatNX logba]

formatNX :: Double -> String
formatNX 0 = ""
formatNX 1 = "n"
formatNX x
    | fromInteger x' == x = printf "n^%d" x'
    | otherwise = printf "n^%.1f" x
  where
    x' = round x

formatLogK :: Int -> String
formatLogK 0 = ""
formatLogK 1 = "log n"
formatLogK k = printf "log^%d n" k
