import           Control.Arrow         ((>>>))
import           Data.Array            (Array, assocs, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':xs <- C.lines <$> C.getContents

    let n = readInt n'

        names :: Array Int [(Int,C.ByteString)]
        names = listArray (1,n) (map (C.words >>> drop 1 >>> zip [1..]) xs)

        dp :: Array (Int,Int) Bool
        dp = assocs names
            & concatMap (\(i,ns) -> map (snd >>> possible i) ns <> replicate (10 - length ns) False)
            & listArray ((1,1),(n,10))
          where
            possible :: Int -> C.ByteString -> Bool
            possible i x
                | i == n    = True
                | otherwise = any (\(j,n) -> x <= n && dp ! (i+1, j)) (names ! (i+1))

        backtrack :: Int -> C.ByteString -> [C.ByteString]
        backtrack i prev
            | i > n     = [prev]
            | otherwise = names ! i
                & find (\(j,y) -> prev <= y && dp ! (i,j))
                & fromJust
                & snd
                & backtrack (i+1)
                & (prev:)

    C.putStr $ case find (\(j,_) -> dp ! (1,j)) (names ! 1) of
        Nothing    -> C.pack "impossible\n"
        Just (_,x) -> C.unlines (backtrack 2 x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
