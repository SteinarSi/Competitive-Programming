import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:_:xs <- C.getContents <&> (C.words >>> map readInt)
    let (good, bad) = splitAt n xs
            & first M.fromAscList
        convert x = case (M.lookupLE x good, M.lookupGE x good) of
            (Just y, Nothing) -> y-x
            (Nothing, Just y) -> y-x
            (Just a, Just b) | b-x <= x-a -> b-x
                             | otherwise  -> a-x
    bad
        & map (convert >>> show)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
