{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Arrow         ((>>>), (&&&))
import           Control.Monad         (ap)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (drop 1 >>> map (,1) >>> M.fromListWith (+)))
        >>> M.fromListWith (M.unionWith (+))
        >>> M.elems
        >>> foldr1 (M.intersectionWith (+))
        >>> M.assocs
        >>> sortOn ((snd >>> negate) &&& fst)
        >>> map fst
        >>> ap (C.unlines >>> bool "ALL CLEAR\n") (null >>> not)
        >>> C.putStr
    )
