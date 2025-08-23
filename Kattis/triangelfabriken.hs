{-# LANGUAGE MultiWayIf #-}

import Data.Functor ((<&>))
import Control.Arrow ((>>>))

main :: IO ()
main = do
    xs <- getContents <&> (words >>> map read)
    putStrLn $ if | any (>90) xs  -> "Trubbig Triangel"
                  | any (==90) xs -> "Ratvinklig Triangel"
                  | all (<90) xs  -> "Spetsig Triangel"
                  | otherwise     -> ""
