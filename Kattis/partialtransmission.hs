import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import qualified Data.IntSet   as S
import           Data.List     (find)

main :: IO ()
main = do
    n:p:xs <- getContents <&> (words >>> map read)
    let Just x = find (`S.notMember` S.fromList xs) [p..n+p-1]
    print x
