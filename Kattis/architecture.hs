import           Data.Array (Array, array, (!))

main :: IO ()
main = do
    r:c:_ <- fmap (map read . words) getLine
    east  <- readSkyline r
    nort  <- readSkyline c
    if placeSkyscrapers nort east r c && placeSkyscrapers east nort c r
        then putStrLn "possible"
        else putStrLn "impossible"

placeSkyscrapers :: Array Int Int -> Array Int Int -> Int -> Int -> Bool
placeSkyscrapers nort east r c = all (\j -> any (\i -> nort ! i >= east ! j) [1..c]) [1..r]

readSkyline :: Int -> IO (Array Int Int)
readSkyline k = array (1, k) . zip [1..] . map read . words <$> getLine
