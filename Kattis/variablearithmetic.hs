{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> filter (/= "+"))
        >>> interpret M.empty
    )

interpret :: M.Map C.ByteString Int -> [[C.ByteString]] -> IO ()
interpret ctx [] = pure ()
interpret ctx (x:xss) = case x of
    ["0"]           -> pure ()
    [var, "=", val] -> interpret (M.insert var (readInt val) ctx) xss
    xs              -> C.putStrLn (eval ctx 0 [] xs) >> interpret ctx xss

eval :: M.Map C.ByteString Int -> Int -> [C.ByteString] -> [C.ByteString] -> C.ByteString
eval _ s [] [] = C.pack (show s)
eval _ 0 vs [] = C.intercalate " + " (reverse vs)
eval _ s vs [] = C.intercalate " + " (C.pack (show s) : reverse vs)
eval ctx s vs (x:xs) = case M.lookup x ctx of
    _ | C.all isDigit x -> eval ctx (s + readInt x) vs xs
    Nothing             -> eval ctx s (x:vs) xs
    Just  y             -> eval ctx (s+y) vs xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
