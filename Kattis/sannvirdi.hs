{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, replicateM)
import           Data.Array            (Array)
import           Data.Array.Base       (listArray, (!))
import           Data.Array.Unboxed    (UArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    (people, guesses) <- replicateM n C.getLine <&> (
            map (C.words >>> (\(a:b:_) -> (a, readInt b)))
            >>> sortOn snd
            >>> ((":(", -1):)
            >>> (\xs -> (listArray (0,n) (map fst xs), listArray (0,n) (map snd xs)))
        )
    ideas <- C.getContents <&> (C.lines >>> tail >>> map readInt)
    mapM_ (solve n people guesses 0 n >>> C.putStrLn) ideas

solve :: Int -> Array Int C.ByteString -> UArray Int Int -> Int -> Int -> Int -> C.ByteString
solve n people guesses lo hi idea | lo == hi = p
                                  | g  == idea = p
                                  | g  < idea && mid == n = p
                                  | g  < idea && gp > idea = p
                                  | g  < idea = solve n people guesses (mid+1) hi idea
                                  | g  > idea = solve n people guesses lo (mid-1) idea
    where mid = (lo + hi) `div` 2
          g = guesses ! mid
          gp = guesses ! (mid+1)
          p = people ! mid

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
