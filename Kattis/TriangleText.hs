{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha, isSpace)
import           Data.Function         ((&))

main :: IO ()
main = C.interact
    $ C.words
        >>> reverse
        >>> map (\x -> x <> bool "" " " (C.last x == '.'))
        >>> split 0
        >>> reverse
        >>> combineLast
        >>> map (C.dropWhileEnd isSpace)
        >>> C.unlines

combineLast :: [(Int,C.ByteString)] -> [C.ByteString]
combineLast [] = []
combineLast [(_,x)] = [x]
combineLast ((p,x):(q,y):xs) | p <= q    = (x <> " " <> y) : map snd xs
                             | otherwise = x : y : map snd xs

split :: Int -> [C.ByteString] -> [(Int,C.ByteString)]
split _ [] = []
split p (x:xs) = (q,ys) : split q zs
  where
    (q,ys,zs) = loop (C.last x == ' ') x xs

    loop :: Bool -> C.ByteString -> [C.ByteString] -> (Int,C.ByteString,[C.ByteString])
    loop k ret [] = (C.length ret - bool 0 2 k ,ret,[])
    loop k ret (x:xs) | l > p     = (l,ret,x:xs)
                      | otherwise = loop k (x <> " " <> ret) xs
      where
        l | k         = C.length ret - 2
          | otherwise = C.length ret
