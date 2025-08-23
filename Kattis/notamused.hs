{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> splitOn "CLOSE"
        >>> zip [1..]
        >>> map (fmap (tail
                >>> map C.words
                >>> count M.empty M.empty
            ) >>> report)
        >>> C.intercalate "\n\n"
        >>> C.putStrLn
    )

count :: M.Map C.ByteString Int -> M.Map C.ByteString Double -> [[C.ByteString]] -> M.Map C.ByteString Double
count times bills [] = bills
count times bills (["ENTER", name, time]:xs) = count (M.insert name (readInt time) times) bills xs
count times bills (["EXIT" , name, time]:xs) = count times (M.insertWith (+) name bill bills) xs
    where
        bill = M.lookup name times
            & fromJust
            & (readInt time -)
            & fromIntegral
            & (* 0.1)

report :: (Int, M.Map C.ByteString Double) -> C.ByteString
report (i, bills) = M.assocs bills
    & map (\(name, bill) -> printf "%s $%.2f" (C.unpack name) bill & C.pack)
    & (C.pack ("Day " ++ show i) :)
    & C.unlines
    & C.init

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
