{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Tuple            (swap)


main :: IO ()
main = C.getContents >>= (
            C.filter isAlpha
        >>> C.inits
        >>> drop 1
        >>> concatMap (C.tails >>> init)
        >>> map (,1)
        >>> M.fromListWith (+)

        >>> M.assocs
        >>> map swap
        >>> sortOn (first negate)
        >>> map (\(c,x) -> C.pack (show c) <> " " <> x)
        >>> C.unlines
        >>> C.putStr
    )
