import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Data.Array.IO         (IOUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           System.Exit           (exitSuccess)
import           System.IO             (BufferMode (LineBuffering),
                                        hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    n <- C.getLine <&> readInt
    seen <- newArray (1,n) False
    dfs n seen

dfs :: Int -> IOUArray Int Bool -> IO ()
dfs n seen = do
    C.getLine
    ds <- C.getLine <&> (C.words >>> map readInt)
    if n `elem` ds
        then print n >> exitSuccess
        else filterM (\e -> readArray seen e >>= \s -> if s then pure False else writeArray seen e True >> pure True) ds
            >>= mapM_ (\e -> print e >> dfs n seen >> print e >> C.getLine >> C.getLine)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
