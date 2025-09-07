import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (..), comparing)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> parse)
        >>> (`scoreboard` M.empty)
        >>> sortOn Down
        >>> map show
        >>> unlines
        >>> putStr
    )

data Team = Team {
    name   :: C.ByteString,
    wins   :: Int,
    draws  :: Int,
    losses :: Int,
    gf     :: Int,
    ga     :: Int
}
    deriving Eq

instance Show Team where
    show t@Team {name, wins, draws, losses, gf, ga} = unwords (C.unpack name : map show [
            wins + draws + losses,
            wins,
            draws,
            losses,
            gf,
            ga,
            diff t,
            points t
        ])

instance Ord Team where
    compare t1 t2 = comparing points t1 t2
                 <> comparing diff t1 t2
                 <> comparing (name >>> Down) t1 t2

instance Semigroup Team where
    (<>) t1 t2 = t1 {
        wins = wins t1 + wins t2,
        draws = draws t1 + draws t2,
        losses = losses t1 + losses t2,
        gf = gf t1 + gf t2,
        ga = ga t1 + ga t2
    }

scoreboard :: [((C.ByteString,Int),(C.ByteString,Int))] -> M.Map C.ByteString Team -> [Team]
scoreboard [] m = M.elems m
scoreboard (((t1,s1),(t2,s2)):xs) m = m
    & record t1 s1 s2
    & record t2 s2 s1
    & scoreboard xs
  where
    record n g1 g2 = M.insertWith (<>) n (Team n (z1 (g1>g2)) (z1 (g1==g2)) (z1 (g1<g2)) g1 g2)
    z1 = bool 0 1

parse :: [C.ByteString] -> ((C.ByteString,Int),(C.ByteString,Int))
parse [t1,s,t2] = ((t1,s1),(t2,s2))
  where
    [s1,s2] = map readInt (C.split '-' s)

diff :: Team -> Int
diff Team {gf, ga} = gf - ga

points :: Team -> Int
points Team {wins, draws} = 3 * wins + draws

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
