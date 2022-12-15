{-# LANGUAGE Strict #-}

import Data.List (genericLength)

target = 2000000

main :: IO ()
main = do
    sensors <- fmap (map parse . map words . lines) (readFile "day15-input.txt")
    let closeSensors = filter (\(Sensor (_,sy) _ d) -> abs (sy - target) <= d) sensors
        max' = maximum $ map (\(Sensor (sx,_) _ d) -> sx+d) sensors
        min' = minimum $ map (\(Sensor (sx,_) _ d) -> sx-d) sensors
    print (genericLength $ filter (canHaveBeacon closeSensors . (,target)) [min'..max'])



data Sensor = Sensor (Int, Int) (Int, Int) Int
    deriving (Show)

canHaveBeacon :: [Sensor] -> (Int, Int) -> Bool
canHaveBeacon [] _ = False
canHaveBeacon (Sensor s@(sx,sy) b@(bx,by) d :xs) p@(x,y) | b == p = False
                                                         | manhattan s p <= d = True
                                                         | otherwise = canHaveBeacon xs p

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

parse :: [String] -> Sensor
parse (_:_:x:y:_:_:_:_:x':y':_) = Sensor (s1, s2) (b1, b2) (manhattan (s1,s2) (b1,b2))
    where d = read . drop 2 . init
          s1 = d x
          s2 = d y
          b1 = d x'
          b2 = read (drop 2 y')