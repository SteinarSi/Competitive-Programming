import           Control.Arrow ((>>>))

main :: IO ()
main = do
    [n,b,s] <- fmap (words >>> map read) getContents
    let (q,r) = quotRem (b+s) n
        (u,v) = (max b s, min b s)
        x | r >= 1    = u-q-1
          | otherwise = u-q
        y | r >= 2    = v-q-1
          | otherwise = v-q
    print (max 0 x + max 0 y)
