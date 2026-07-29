import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,x] <- C.getContents <&> C.lines
    C.elemIndices (C.head x) xs
        & map show
        & unlines
        & putStr
