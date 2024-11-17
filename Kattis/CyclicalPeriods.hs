import           Control.Arrow         (second, (&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)


main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.readInt >>> fromJust >>> second C.tail)
        >>> (\xs -> runST (newArray ('a','z') (-1) >>= solulu (-1,'a') xs))
        >>> pure
        >>> putStrLn
    )

solulu :: (Int,Char) -> [(Int,C.ByteString)] -> STUArray s Char Int -> ST s Char
solulu (_,c) []            pos = pure c
solulu best  ((i,xs'):xss) pos = insert best (C.unpack xs')
    where
        insert (b,c) [] = solulu (b,c) xss pos
        insert (b,c) (x:xs) = do
            prev <- readArray pos x
            writeArray pos x i
            if prev == -1
                then insert best xs
                else if i - prev > b
                    then insert (i-prev,x) xs
                    else insert (b,c) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
