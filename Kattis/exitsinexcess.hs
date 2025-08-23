import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,m):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let (red,blue) = zip [1..] xs
            & partition (snd >>> uncurry (<))
            & map fst *** map fst
        delete | length red < length blue = red
               | otherwise = blue

    delete
        & (length delete:)
        & map show
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
