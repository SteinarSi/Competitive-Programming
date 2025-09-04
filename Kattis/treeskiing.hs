{-# LANGUAGE LambdaCase #-}

import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    (l,r) <- getContents <&> (words >>> drop 1 >>> foldl (\(l,r) -> \case "N" -> (l+1,r); "W" -> (l,r+1)) (0,0))
    print (choose (r+l) l - 1)

choose :: Int -> Int -> Int
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
