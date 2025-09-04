import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:_:xs <- C.getContents <&> C.words

    let (ys,zs) = splitAt (readInt n) xs
            & second ((`zip` map show [1..]) >>> M.fromList)

    ys
        & map (flip (M.findWithDefault "stolen!") zs)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
