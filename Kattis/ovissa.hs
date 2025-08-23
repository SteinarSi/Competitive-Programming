main :: IO ()
main = interact (show . length . filter ('u'==))