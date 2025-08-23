import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M

main :: IO ()
main = do
    _:sentence:_:xs <- C.getContents <&> (C.lines >>> map C.words)

    let dict = xs
            & map (head &&& last)
            & M.fromList

    sentence
        & map (dict M.!)
        & C.unwords
        & C.putStrLn
