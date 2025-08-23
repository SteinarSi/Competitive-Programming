import           Control.Arrow         ((>>>), (&&&))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, runSTUArray, writeArray)
import           Data.Array.Base       (UArray, listArray, readArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (index, range)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ncsf1:sigma:fs1:rest <- C.getContents <&> C.lines
    let [n1,c,s1,f1] = C.words ncsf1
            & map readInt
        (tss1,ncsf2:_:fs2:tss2) = splitAt n1 rest
        [n2,_,s2,f2] = C.words ncsf2
            & map readInt
        table1 = parseTable n1 c tss1
        table2 = parseTable n2 c tss2
        accept1 = parseAccept n1 fs1
        accept2 = parseAccept n2 fs2

        finals = C.words fs1
            & map readInt

        rng = ((1,1),(n1,n2))
        n9 = n1*n2
        s9 = index rng (s1,s2) + 1
        f9 = f1*f2
        fs9 = range rng
            & filter (\(i,j) -> accept1 ! i && accept2 ! j)
            & map (index rng >>> succ >>> show >>> C.pack)
            & C.unwords

    [n9,c,s9,f9]
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn
    C.putStrLn sigma
    C.putStrLn fs9

    range rng
        & map ((\(u1,u2) -> [1..c]
            & map ((
                ((u1,) >>> (table1!))
                    &&&
                ((u2,) >>> (table2!))
                ) >>> index rng >>> succ >>> show >>> C.pack
                )
            ) >>> C.unwords)
        & C.unlines
        & C.putStr

parseTable :: Int -> Int -> [C.ByteString] -> UArray (Int,Int) Int
parseTable n c = concatMap (C.words >>> map readInt) >>> listArray ((1,1),(n,c))

parseAccept :: Int -> C.ByteString -> UArray Int Bool
parseAccept n fs = runSTUArray $ do
        accept <- newArray (1,n) False
        mapM_ (readInt >>> flip (writeArray accept) True) (C.words fs)
        pure accept

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
