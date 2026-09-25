{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    [pattern,word] <- T.lines <$> TIO.getContents

    let possible = case T.findIndex (=='*') pattern of
            Nothing -> pattern == word
            Just  i -> let jokers = T.count "*" pattern
                           (q,r) = quotRem (T.length word - T.length pattern + jokers) jokers
                        in r == 0 && T.replace "*" (T.take q (T.drop i word)) pattern == word

    putStrLn $ if possible
        then "yes"
        else "no"
