{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [a, b] <- C.getLine <&> (C.words >>> map readInt)
    let pairs = do
            x <- [a..b]
            y <- [x..b]
            guard (sort (digits x <> digits y) == sort (digits (x*y)))
            pure (format x y)
    C.putStrLn (show' (length pairs) <> " digit-preserving pair(s)")
    mapM_ C.putStrLn pairs

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

{-# INLINE format #-}
format :: Int -> Int -> C.ByteString
format x y = "x = " <> show' x <> ", y = " <> show' y <> ", xy = " <> show' (x*y)

show' :: Show s => s -> C.ByteString
show' = show >>> C.pack

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
