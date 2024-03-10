{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, p, m] <- C.getLine <&> (C.words >>> map readInt)
    players <- replicateM n C.getLine <&> foldr (`M.insert`0) M.empty
    C.getContents >>= (
                C.lines
            >>> map (C.words >>> (\(a:b:_) -> (a, readInt b)))
            >>> play p players []
            >>> mapM_ C.putStrLn
        )

play :: Int -> M.Map C.ByteString Int -> [C.ByteString] -> [(C.ByteString, Int)] -> [C.ByteString]
play p scores [] [] = ["No winner!"]
play p scores ws [] = reverse ws & map (<> " wins!")
play p scores ws ((w,x):xs) = case M.lookup w scores of
    Nothing            -> play p scores ws xs
    Just y | y+x >= p  -> play p (M.delete w scores) (w:ws) xs
           | otherwise -> play p (M.insert w (y+x) scores) ws xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
