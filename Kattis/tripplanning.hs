import           Control.Arrow         ((>>>))
import           Control.Monad         (when, zipWithM_)
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import           Data.Array.Unboxed    (elems)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let route = elems $ runSTUArray $ do
            ret <- newArray (1,n) (-1)
            zipWithM_ (\i [u,v] -> do
                when (u+1 == v || u == n && v == 1) (writeArray ret u i)
                when (v+1 == u || v == n && u == 1) (writeArray ret v i)
                ) [1..m] xs
            pure ret

    putStr $ if -1 `elem` route
        then "impossible\n"
        else unlines (map show route)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
