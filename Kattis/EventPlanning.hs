{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, b, h, w] <- C.getLine <&> (C.words >>> map readInt)

    hotels <- replicateM h readHotel <&> (
                filter (fst >>> (n*) >>> (<=b))
            >>> filter (snd >>> any (>=n))
        )

    C.putStrLn $ if null hotels
        then "stay home"
        else map (fst >>> (n*)) hotels
            & minimum
            & show
            & C.pack

readHotel :: IO (Int, [Int])
readHotel = do
    cost <- C.getLine <&> readInt
    beds <- C.getLine <&> (C.words >>> map readInt)
    pure (cost, beds)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
