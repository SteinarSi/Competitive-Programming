import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Functor          ((<&>))
import           Data.List             (find)

main :: IO ()
main = do
    xs <- C.getLine
    print $ runST (newArray (('a',0),('z',C.length xs-1)) (-1) >>= alphabet xs)

alphabet :: forall s. C.ByteString -> STUArray s (Char,Int) Int -> ST s Int
alphabet xs dp = alpha ('a',0)
    where
        alpha :: (Char,Int) -> ST s Int
        alpha (c,i) | c > 'z'          = pure 0
                    | i >= C.length xs = pure (ord 'z' - ord c + 1)
                    | otherwise = do
                        d <- readArray dp (c,i)
                        if d /= -1
                            then pure d
                            else do
                                skip <- alpha (succ c, i) <&> succ
                                val <- case elemIndexFrom c i xs of
                                        Nothing -> pure skip
                                        Just ix -> alpha (succ c, ix) <&> min skip
                                writeArray dp (c,i) val
                                pure val

elemIndexFrom :: Char -> Int -> C.ByteString -> Maybe Int
elemIndexFrom c i xs = find (\j -> C.index xs j == c) [i..C.length xs-1]
