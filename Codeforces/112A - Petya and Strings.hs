import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys] <- C.getContents <&> (C.map toLower >>> C.lines)

    print $ case compare xs ys of
        LT -> -1
        EQ ->  0
        GT ->  1
