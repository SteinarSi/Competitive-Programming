{-# LANGUAGE LambdaCase #-}

import           Control.Arrow ((***), (>>>))
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.List     (foldl', partition)
import           Data.Maybe    (mapMaybe)

type Lit = (Bool,Int)
type Clause = [Lit]

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map words
        >>> parse
        >>> map (satisfiable >>> bool "unsatisfiable" "satisfiable")
        >>> unlines
        >>> putStr
    )

satisfiable :: [Clause] -> Bool
satisfiable xs = case normalize xs of
    Nothing -> False
    Just [] -> True
    Just ys@(((n,x):_):_) -> maybe False satisfiable (set (x,not n) ys) || maybe False satisfiable (set (x,n) ys)

normalize :: [Clause] -> Maybe [Clause]
normalize xs | null ss = Just xs
             | otherwise = foldl' (\ret s -> ret >>= set s) (Just xs) ss >>= normalize
  where
    ss = mapMaybe (\case [(n,x)] -> Just (x,not n); _ -> Nothing) xs

set :: (Int,Bool) -> [Clause] -> Maybe [Clause]
set _ [] = Just []
set (x,s) (xs:xss)
    | any (fst >>> (/=s)) ys = set (x,s) xss
    | null zs = Nothing
    | otherwise = (zs:) <$> set (x,s) xss
  where
    (ys,zs) = partition (snd >>> (==x)) xs

parse :: [[String]] -> [[Clause]]
parse [] = []
parse ([n,m]:xs) = splitAt (read m) xs
    & map parseClause *** parse
    & uncurry (:)
  where
    parseClause :: [String] -> Clause
    parseClause []               = []
    parseClause ("v":yss)        = parseClause yss
    parseClause (('~':_:ys):yss) = (True,read ys) : parseClause yss
    parseClause ((_:ys):yss)     = (False,read ys) : parseClause yss
