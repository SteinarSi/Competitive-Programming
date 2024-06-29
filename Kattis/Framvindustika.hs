{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Maybe               (fromJust)
import           Data.Functor             ((<&>))
import           Text.Printf              (printf)

main :: IO ()
main = do
    p:w:_ <- C.getContents <&> (C.words >>> map readInt)

    C.putStrLn $ C.concat [
            "[",
            C.replicate (floor (fromIntegral (p*w) / 100)) '#',
            C.replicate (ceiling (fromIntegral ((100-p) * w) / 100)) '-',
            "] | ",
            C.pack (printf "%3d" p),
            "%"
        ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
