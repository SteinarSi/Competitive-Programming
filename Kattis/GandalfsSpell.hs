{-# LANGUAGE TupleSections #-}

import qualified Data.Map   as M
import           Data.Maybe (catMaybes)

main :: IO ()
main = do
    string <- getLine
    sentence <- fmap words getLine
    print $ length string == length sentence && gandalf M.empty M.empty (zip string sentence)

gandalf :: M.Map Char String -> M.Map String Char -> [(Char,String)] -> Bool
gandalf c2w w2c [] = True
gandalf c2w w2c (cw@(c,word):xs)
    | any (/=cw) (catMaybes [(c,) <$> M.lookup c c2w, (,word) <$> M.lookup word w2c]) = False
    | otherwise = gandalf (M.insert c word c2w) (M.insert word c w2c) (filter (/=cw) xs)
