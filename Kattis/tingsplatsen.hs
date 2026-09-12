import           Control.Arrow ((>>>))
import           Data.Array    (range)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Maybe    (fromJust)
import qualified Data.Set      as S

main :: IO ()
main = do
    n':m':xss <- getContents <&> words

    let n = read n'
        m = read m'

        h:hs = do
            (i,xs) <- zip [1..n] xss
            (j,'*') <- zip [1..m] xs
            pure (i,j)

        squares = range ((1,1),(n,m))
            & filter (\t -> all (manhattan t >>> (== manhattan t h)) hs)
            & S.fromList

    zipWith (\i -> zipWith (\j x -> if S.member (i,j) squares then 'X' else x) [1..m]) [1..n] xss
        & unlines
        & putStr

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)
