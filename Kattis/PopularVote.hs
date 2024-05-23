{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- C.getLine <&> readInt
    replicateM_ t $ do
        n <- C.getLine <&> readInt
        xs <- replicateM n C.getLine <&> map readInt

        let (c,i) = xs
                & (`zip` [1..])
                & maximum
            j = elemIndex c xs
                & fromJust
                & succ
            s = sum xs

        C.putStrLn $ if
            | c > s `div` 2 -> "majority winner " <> C.pack (show i)
            | i /= j        -> "no winner"
            | otherwise     -> "minority winner " <> C.pack (show i)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
