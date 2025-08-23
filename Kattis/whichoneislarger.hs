{-# LANGUAGE MultiWayIf #-}

import Control.Arrow ((>>>), (***))
import Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y] <- getContents <&> lines

    let (xf,yf) = (read x, read y :: Double)
        (xt,yt) = (parseTuple x, parseTuple y)

    putStrLn $ if 
        | xf > yf && xt > yt -> x
        | yf > xf && yt > xt -> y
        | otherwise          -> "-1"

parseTuple :: String -> (Int,Int)
parseTuple = span (/='.') >>> read *** (drop 1 >>> read)
