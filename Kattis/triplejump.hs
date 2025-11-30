import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:a':b':xs <- C.getContents <&> (C.words >>> map readInt)
    let a = a' `div` 3
        b = b' - a' + a
        c = last xs `div` 3
    putStrLn (unwords (map show [a,b,c]))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
