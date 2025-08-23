{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Read, Ord, Eq)

data Date = Date Month Int
    deriving (Eq, Ord)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> (\(a:b:_) -> solulu (Date (read (C.unpack b)) (readInt a)))
            >>> C.putStrLn
            )
    )

solulu :: Date -> C.ByteString
solulu date | date <= Date Jan 20 = "Capricorn"
            | date <= Date Feb 19 = "Aquarius"
            | date <= Date Mar 20 = "Pisces"
            | date <= Date Apr 20 = "Aries"
            | date <= Date May 20 = "Taurus"
            | date <= Date Jun 21 = "Gemini"
            | date <= Date Jul 22 = "Cancer"
            | date <= Date Aug 22 = "Leo"
            | date <= Date Sep 21 = "Virgo"
            | date <= Date Oct 22 = "Libra"
            | date <= Date Nov 22 = "Scorpio"
            | date <= Date Dec 21 = "Sagittarius"
            | otherwise           = "Capricorn"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>>  fst
