import           Control.Arrow         (second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

main :: IO ()
main = do
    _:[start]:xs <- C.getContents <&> (C.lines >>> map C.words)
    let graph = M.fromListWith (<>) (map (\[s,t] -> (s,[t])) xs)
    search start graph S.empty 1 (S.singleton start)
        & maybe "NO BLACK HOLE" show
        & putStrLn

search :: C.ByteString -> M.Map C.ByteString [C.ByteString] -> S.Set C.ByteString -> Int -> S.Set C.ByteString -> Maybe Int
search start graph seen i curr
    | S.null curr = Nothing
    | S.member start next = Just i
    | otherwise = search start graph (seen <> next) (i+1) next
  where
    next = S.fromList (concatMap ((\x -> M.findWithDefault [] x graph) >>> filter (`S.notMember` seen)) curr)
