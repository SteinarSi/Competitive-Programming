import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, zipWithM_)
import           Data.Array.Base       (getElems, readArray, (!))
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Set              as S

main :: IO ()
main = do
    (training,password) <- C.getContents <&> (C.lines >>> drop 1 >>> init &&& last)

    let mapping = runSTUArray $ do
            ret <- newArray ('a','z') '\NUL'
            training
                & concatMap (C.words >>> \[xs,ys] -> C.zipWith (writeArray ret) xs ys)
                & sequence_
            unusedKeys <- filterM (readArray ret >>> fmap (=='\NUL')) ['a'..'z']
            unusedValues <- getElems ret <&> (S.fromList >>> S.difference (S.fromList ['a'..'z']) >>> S.toList)
            zipWithM_ (writeArray ret) unusedKeys unusedValues
            pure ret

    password
        & C.map (mapping!)
        & C.putStrLn

