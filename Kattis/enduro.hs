import           Control.Arrow         ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M

limit :: Int
limit = 3 * 60 * 60

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> (head >>> parseTime >>> fst >>> parseLap >>> map)
            &&&
            drop 2
        >>> uncurry ($)
        >>> M.fromListWith record
        >>> M.assocs
        >>> filter (snd >>> snd >>> (>=limit))
        >>> sortOn (\(_,(l,t)) -> (-l,t))
        >>> map (\(n,(l,_)) -> C.pack (show l <> " ") <> n)
        >>> C.unlines
        >>> C.putStr
    )

record :: (Int,Int) -> (Int,Int) -> (Int,Int)
record (_,new) (l,prev) | prev >= limit = (l,prev)
                        | otherwise     = (l+1,new)

parseLap :: Int -> C.ByteString -> (C.ByteString,(Int,Int))
parseLap start xs | start <= time = (name,(1,time-start))
                  | otherwise     = (name,(1,time+12*60*60-start))
    where (time,name) = parseTime xs

parseTime :: C.ByteString -> (Int,C.ByteString)
parseTime xs = (seconds + 60 * minutes + 60 * 60 * hours, C.drop 4 rest3)
    where
        Just (hours,rest1) = C.readInt xs
        Just (minutes,rest2) = C.readInt (C.tail rest1)
        Just (seconds,rest3) = C.readInt (C.tail rest2)
