{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Functor          ((<&>))
import           Data.Map              (Map, findWithDefault, fromList)
import           Data.Maybe            (fromJust)

data Form = C | M
    deriving (Read, Eq)

main :: IO ()
main = do
    [n,p] <- C.getLine <&> (C.words >>> map readInt)
    nouns <- replicateM n C.getLine <&> (
                map C.words
            >>> map (\[a,b] -> (a, read (map toUpper (C.unpack b))))
            >>> fromList
        )
    C.getContents >>= (
                C.lines
            >>> mapM_ (
                    C.words
                >>> (\xs -> (C.unwords (init xs), findWithDefault C (last xs) nouns))
                >>> (\(phrase,cm) -> findWithDefault cm phrase phrases == cm)
                >>> bool "Not on my watch!" "Correct!"
                >>> C.putStrLn
            )
        )

phrases :: Map C.ByteString Form
phrases = fromList [
        ("number of", C),
        ("amount of", M),
        ("fewest", C),
        ("least", M),
        ("fewer", C),
        ("less", M),
        ("many", C),
        ("much", M),
        ("few", C),
        ("little", M)
    ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
