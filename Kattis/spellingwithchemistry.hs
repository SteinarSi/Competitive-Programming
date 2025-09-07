import           Control.Arrow         ((***), (>>>))
import           Data.Array            (listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.words

    let (table,queries) = map (C.map toLower) rest
            & splitAt (readInt n)
            & S.fromList *** drop 1

        spell :: C.ByteString -> Int
        spell xs = dp ! 0
          where
            m = C.length xs
            dp = listArray (0,m) (map f [0..m])
            f i = case compare i m of
                GT -> 0
                EQ -> 1
                LT -> [1..min 5 (m-i)]
                    & filter (\j -> C.take j (C.drop i xs) `S.member` table)
                    & map ((i+) >>> (dp!))
                    & sum
    queries
        & map (spell >>> show)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
