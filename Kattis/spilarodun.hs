{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (sortBy)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> (last >>> C.words >>> sorter) &&& (init >>> map parse)
        >>> uncurry sortBy
        >>> map name
        >>> C.unlines
        >>> C.putStr
    )

data Card = Card {
    name :: C.ByteString,
    uid  :: Int,
    kind :: Int,
    date :: (Int,Int,Int)
} deriving (Show)

sorter :: [C.ByteString] -> Card -> Card -> Ordering
sorter []                _ _ = EQ
sorter ("nafn"      :xs) x y = on compare name x y <> sorter xs x y
sorter ("id"        :xs) x y = on compare uid  x y <> sorter xs x y
sorter ("flokkur"   :xs) x y = on compare kind x y <> sorter xs x y
sorter ("dagsetning":xs) x y = on compare date x y <> sorter xs x y

parse :: C.ByteString -> Card
parse line = Card {
        name = name,
        uid  = readInt (C.drop 1 uid),
        kind = kinds M.! kind,
        date = (y, m, d)
    }
  where
    [name,uid,kind,date] = C.splitWith (==',') line
    [y,m,d] = map readInt (C.splitWith (=='-') (C.drop 1 date))

kinds :: M.Map C.ByteString Int
kinds = M.fromList $ flip zip [0..] $ concat [
        map (" Skrimsli - " <>) ["Venjulegt","Ahrifa","Bodunar","Samruna","Samstillt","Thaeo","Penduls","Tengis"],
        map (" Galdur - " <>) ["Venjulegur","Bunadar","Svida","Samfelldur","Bodunar","Hradur"],
        map (" Gildra - " <>) ["Venjuleg","Samfelld","Mot"],
        [" Annad"]
    ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
