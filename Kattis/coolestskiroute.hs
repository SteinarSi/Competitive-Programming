import           Control.Arrow         ((>>>))
import           Data.Array            (elems, listArray, (!))
import           Data.Array.ST         (newArray, readArray, runSTArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (1,n) []
            mapM_ (\[s,t,c] -> readArray ret s >>= (((t,c):) >>> writeArray ret s)) xs
            pure ret

        dp = [1..n]
            & map ((graph !) >>> map (\(t,c) -> dp ! t + c) >>> (0:) >>> maximum)
            & listArray (1,n)

    print (maximum (elems dp))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
