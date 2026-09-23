import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

data Player = Player {
    name  :: C.ByteString,
    prefs :: [C.ByteString]
}
    deriving (Eq,Ord)

main :: IO ()
main = do
    nmk:xs <- C.getContents <&> (C.lines >>> map C.words)
    let [n,m,_] = map readInt nmk
    splitAt n xs
        & (map (\(team:players) -> (team, (M.fromList (zip players [0..]), S.empty))) >>> M.fromList)
            ***
            map (\(name:prefs) -> Player name prefs)
        & uncurry (draft m)
        & C.putStr

draft :: Int -> M.Map C.ByteString (M.Map C.ByteString Int, S.Set (Int,Player)) -> [Player] -> C.ByteString
draft m teams [] = M.assocs teams
    & map (\(team,(_,players)) -> C.unwords (team : map (snd >>> name) (S.toList players)))
    & C.unlines
draft m teams ((Player _ []):xs) = draft m teams xs
draft m teams (p@Player {name,prefs = (t:ts)}:xs)
    | S.size picks' <= m = draft m (M.insert t (rank,picks') teams) xs
    | otherwise          = let ((_, Player name' (_:ts')), picks'') = S.deleteFindMax picks'
                            in draft m (M.insert t (rank,picks'') teams) (Player name' ts' : xs)
  where
    (rank,picks) = teams M.! t
    picks' = S.insert (rank M.! name, p) picks

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
