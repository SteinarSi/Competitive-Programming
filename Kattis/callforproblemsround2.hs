import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import qualified Data.IntSet   as S

main :: IO ()
main = do
    _:k:xs <- getContents <&> (words >>> map read)
    print (min k (S.size (S.fromList xs)))
