import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ns:xs <- C.getContents <&> (C.lines >>> map C.words)
    let x = readInt (last ns)
    xs
        & map (\[n,a,b] -> (n,(readInt a, readInt b)))
        & sortBy (\(_,q1) (_,q2) -> compare (reward q1 (reward q2 x)) (reward q2 (reward q1 x)))
        & map fst
        & C.unlines
        & C.putStr

reward :: (Int,Int) -> Int -> Int
reward (a,b) x = x + a*x + b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
