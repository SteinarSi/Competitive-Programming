import           Data.Char (isControl)

main :: IO ()
main = do
    getLine
    getLine
    xs <- fmap (filter (not . isControl)) getContents
    let dots = fromIntegral $ length $ filter ('.' ==) xs
    print (dots / fromIntegral (length xs))
