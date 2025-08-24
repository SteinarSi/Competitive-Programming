import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STArray, getElems, newListArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,p]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (piles,moves) = splitAt p rest
            & map (drop 1 >>> reverse) *** drop 1
        result = runST $ do
            ps <- newListArray (1,p) piles :: ST s (STArray s Int [Int])
            forM_ moves $ \[s,d,n] -> do
                (xs,xs') <- readArray ps s <&> splitAt n
                writeArray ps s xs'
                ys <- readArray ps d
                writeArray ps d (xs <> ys)
            getElems ps <&> (map (reverse >>> map show >>> unwords) >>> unlines)

    putStr result

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
