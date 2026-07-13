import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:items:order:cats <- C.getContents <&> (C.lines >>> map C.words)

    let cat2ix = M.fromList (zip order [0..])
        item2ix = M.fromList (concatMap (\(c:_:xs) -> map (,cat2ix M.! c) xs) cats)

    items
        & sortOn (item2ix M.!)
        & C.unwords
        & C.putStrLn
