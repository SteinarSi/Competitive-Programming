import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)


main :: IO ()
main = do
    [n,m] <- C.getLine <&> (C.words >>> map readInt)
    pieces <- C.getContents <&> (C.lines >>> map (C.words >>> tail >>> map readInt) >>> concat)
    putStrLn $ runST $ do
        seen <- newArray (1,m) False :: ST s (STUArray s Int Bool)
        forM_ pieces $ flip (writeArray seen) True
        getElems seen <&> (and >>> bool "Neibb" "Jebb")

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
