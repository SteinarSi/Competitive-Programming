{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nmst:xs <- C.getContents <&> (C.lines >>> init)

    let [n,_,s,t] = C.words nmst
        mapping = M.fromList (C.zip s t)

    xs
        & map (C.map (\c -> M.findWithDefault c c mapping) >>> blanks)
        & concatenate (readInt n) ""
        & C.unlines
        & C.putStr

concatenate :: Int -> C.ByteString -> [C.ByteString] -> [C.ByteString]
concatenate n r [] | C.null r = []
                   | otherwise = [r]
concatenate n r (x:xs) | C.length r + C.length x <= n = concatenate n (r <> x) xs
                       | otherwise = r : concatenate n x xs

blanks :: C.ByteString -> C.ByteString
blanks = C.groupBy (\a b -> (a==' ') == (b==' '))
    >>> map (\x ->
        if | C.null x        -> x
           | C.head x /= ' ' -> x
           | C.length x < 4  -> x
           | C.length x < 10 -> C.pack ('&' : '0' : show (C.length x))
           | otherwise       -> C.pack ('&' : show (C.length x))
        )
    >>> C.concat

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
