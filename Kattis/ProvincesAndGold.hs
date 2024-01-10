main :: IO ()
main = do
    gold:silver:copper:_ <- fmap (map read . words) getLine
    let buyingPower = 3 * gold + 2 * silver + 1 * copper
    putStrLn (bestVictory buyingPower ++ bestTreasure buyingPower)

bestVictory :: Int -> String
bestVictory x | x >= 8    = "Province or "
              | x >= 5    = "Duchy or "
              | x >= 2    = "Estate or "
              | otherwise = ""

bestTreasure :: Int -> String
bestTreasure x | x >= 6    = "Gold"
               | x >= 3    = "Silver"
               | otherwise = "Copper"
