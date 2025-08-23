import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = do
    nk:rest <- C.getContents <&> C.lines

    let [n,k] = map readInt (C.words nk)
        (picks,ranking) = splitAt n rest
            & map (C.words >>> drop 1 >>> map (playerToId M.!) >>> (,[]))
                ***
              (drop 1 >>> zip [1..])
        playerToId = M.fromList (map swap ranking)
        idToPlayer = M.fromList ranking

    S.fromList (M.elems playerToId)
        & draft k [] picks
        & map (map (idToPlayer M.!) >>> C.unwords)
        & C.unlines
        & C.putStr

draft :: Int -> [([Int], [Int])] -> [([Int], [Int])] -> S.IntSet -> [[Int]]
draft 0 _ ret _                = map (snd >>> reverse) ret
draft k ret [] players         = draft (k-1) [] (reverse ret) players
draft k ret (([],r):xs) ps = let (p,ps') = S.deleteFindMin ps
                             in  draft k (([],p:r):ret) xs ps'
draft k ret ((c:cs,r):xs) ps | S.member c ps = draft k ((cs,c:r):ret) xs (S.delete c ps)
                             | otherwise     = draft k ret ((cs,r):xs) ps

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
