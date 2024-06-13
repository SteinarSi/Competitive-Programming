{-# LANGUAGE TypeApplications #-}

import           Control.Arrow ((>>>))
import           Data.Function ((&))

main :: IO ()
main = do
    [n, k] <- fmap (words >>> map (read @Integer)) getContents

    let possible = fromIntegral n
            & logBase 2
            & ceiling
            & (<=k)

    putStrLn $ if possible
        then "Your wish is granted!"
        else "You will become a flying monkey!"
