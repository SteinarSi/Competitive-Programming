{-# LANGUAGE BangPatterns #-}

import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    n:_:xs <- C.getContents <&> (C.words >>> map readInt)

    let (tasks, times) = splitAt n xs
            & sortOn Down *** sortOn Down

    print $ work 0 tasks times

work :: Int -> [Int] -> [Int] -> Int
work !r [] _ = r
work !r _ [] = r
work !r (x:xs) (y:ys) | x <= y    = work (r+1) xs ys
                      | otherwise = work r     xs (y:ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
