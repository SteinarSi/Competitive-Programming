import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set      as S

main :: IO ()
main = do
    [[a,b],[c,d]] <- lines <$> getContents
    print $ bfs S.empty (Seq.singleton (0,(a,b,c,d)))

bfs :: S.Set (Char,Char,Char,Char) -> Seq (Int,(Char,Char,Char,Char)) -> Int
bfs seen Empty = error "bruh"
bfs seen ((i,('1','2','3','-')) :<| _) = i
bfs seen ((i,u) :<| xs) = bfs (foldr S.insert seen ys) (xs <> Seq.fromList (map (i+1,) ys))
  where
    ys = filter (`S.notMember` seen) (moves u)

moves :: (Char,Char,Char,Char) -> [(Char,Char,Char,Char)]
moves (a,b,c,'-') = [(a,'-',c,b),(a,b,'-',c)]
moves (a,b,'-',c) = [('-',b,a,c),(a,b,c,'-')]
moves (a,'-',b,c) = [(a,c,b,'-'),('-',a,b,c)]
moves ('-',a,b,c) = [(b,a,'-',c),(a,'-',b,c)]

