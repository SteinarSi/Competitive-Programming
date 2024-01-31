import           Data.Char (chr, ord)

main :: IO ()
main = do
    notes <- fmap (tail . words) getContents
    let scales = filter (\key -> all (`elem` scale key) notes) keys
    putStrLn $ case scales of
        [] -> "none"
        xs -> unwords xs

keys :: [String]
keys = take 12 (iterate semitone "A#")

scale :: String -> [String]
scale start = map ($ start) $ scanl (.) id [tone, tone, semitone, tone, tone, tone, semitone]

tone :: String -> String
tone = semitone . semitone

semitone :: String -> String
semitone "B"      = "C"
semitone "E"      = "F"
semitone [x, '#'] = [next x]
semitone [x]      = [x, '#']

next :: Char -> Char
next = chr . (+ord 'A') . (`mod` 7) . succ . subtract (ord 'A') . ord
