main :: IO ()
main = getLine >> getContents >>= print . sum . map yarn

yarn :: Char -> Int
yarn '.'  = 20
yarn 'O'  = 10
yarn '\\' = 25
yarn '/'  = 25
yarn 'A'  = 35
yarn '^'  = 5
yarn 'v'  = 22
yarn _    = 0
