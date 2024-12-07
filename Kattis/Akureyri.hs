{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl')
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> zip [1..]
        >>> filter (fst >>> even)
        >>> map (snd >>> (,1))
        >>> M.fromListWith (+)
        >>> M.toAscList
        >>> map (\(x,y) -> x <> " " <> C.pack (show y))
        >>> C.unlines
        >>> C.putStr
    )
