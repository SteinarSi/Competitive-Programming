import           Control.Arrow         (first, (&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> solve
    )

solve :: [C.ByteString] -> IO ()
solve [] = pure ()
solve (x:xs) = splitAt (readInt x) xs
    & (map (C.words >>> (\[toy,count] -> (toy, readInt count)))
            >>> M.fromListWith (+)
            >>> (M.size &&& M.toList)
            >>>
                (show >>> C.pack)
                    ***
                (sortOn (swap >>> first negate) >>> map (\(t,c) -> C.unwords [t, C.pack (show c)]))
            >>> uncurry (:)
            >>> C.unlines
            >>> C.init
            >>> C.putStrLn
    ) *** solve
    & uncurry (>>)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
