main = interact (show . length . filter (('-'==) . head) . words)