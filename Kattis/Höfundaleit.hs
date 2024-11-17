import           Control.Arrow         (first, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (groupBy, sort)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, fromMaybe)
import qualified Data.Set              as S
import           Data.Tuple            (swap)

main :: IO ()
main = do
    n:xs' <- C.getContents <&> C.lines
    let ((psum, book2author),queries) = splitAt (readInt n) xs'
            & first (map (C.words >>> (last &&& (head >>> C.init)))
                >>> sort
                >>> (groupBy (on (==) fst) >>> prefixSum 1)
                        &&&
                    (map swap >>> M.fromList)
            )

    queries
        & map (solve psum book2author
            >>> fromMaybe (-1)
            >>> show
            >>> C.pack
        )
        & C.unlines
        & C.putStr

solve :: M.Map C.ByteString (Int, S.Set C.ByteString) -> M.Map C.ByteString C.ByteString -> C.ByteString -> Maybe Int
solve psum book2author book = do
    author <- M.lookup book book2author
    (authIx, books)  <- M.lookup author psum
    bookIx <- S.lookupIndex book books
    pure (authIx + bookIx)

prefixSum :: Int -> [[(C.ByteString, C.ByteString)]] -> M.Map C.ByteString (Int, S.Set C.ByteString)
prefixSum prev []                      = M.empty
prefixSum prev (xs@((author,_):_):xss) = M.insert author (prev, S.fromList (map snd xs)) $ prefixSum (prev + length xs) xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
