import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as S

data Query = A Int
           | D Int
           | F Int Int
    deriving Read

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> read)
        >>> solve S.empty
        >>> unlines
        >>> putStr
    )

solve :: S.Set Int -> [Query] -> [String]
solve _        []           = []
solve schedule (A t   : xs) = solve (S.insert t schedule) xs
solve schedule (D t   : xs) = solve (S.delete t schedule) xs
solve schedule (F f t : xs) = show ((t-f+1) - (gt-lt-1)) : solve schedule xs
  where
    lt = S.lookupLT f schedule
        & maybe (-1) (`S.findIndex` schedule)
    gt = S.lookupGT t schedule
        & maybe (S.size schedule) (`S.findIndex` schedule)
