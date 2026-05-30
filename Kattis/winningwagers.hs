{-# LANGUAGE LambdaCase #-}

import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [_,xs,l] <- getContents <&> lines
    words xs
        & map (\case "COIN" -> 2; "DIE" -> 6; "CARDS" -> 52)
        & product
        & pred
        & (read l*)
        & print
