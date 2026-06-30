import           Control.Arrow         ((>>>))
import           Control.Monad         (zipWithM_)
import           Data.Array.ST         (newArray, range, readArray, runSTArray,
                                        writeArray)
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = do
    [n,m]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        tune :: UArray Int Int
        tune = listArray (1,m) (last rest)

        note2instrument :: Array Int S.IntSet
        note2instrument = runSTArray $ do
            ret <- newArray (0,1000) S.empty
            zipWithM_ (\i -> drop 1 >>> mapM_ (\t -> readArray ret t >>= (S.insert i >>> writeArray ret t))) [1..n] (init rest)
            pure ret

        dp :: Array (Int,Int) (Maybe Int)
        dp = listArray rng (map f (range rng))
          where
            rng = ((0,1),(m,n))

            f (0,_) = Just 0
            f (t,i)
                | i `S.member` (note2instrument ! (tune ! t)) = dp ! (t-1,i)
                | null options = Nothing
                | otherwise = Just (minimum options + 1)
              where
                options = note2instrument ! (tune ! t)
                    & S.toList
                    & mapMaybe ((t-1,) >>> (dp!))

    note2instrument ! (tune ! m)
        & S.toList
        & mapMaybe ((m,) >>> (dp!))
        & minimum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
