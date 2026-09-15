import           Control.Arrow         (first, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 2 >>> map (C.split ',' &&& id))

    let incongruousity x = maximum (map (fst >>> zipWith (/=) x >>> filter id >>> length) xs)
        alternatives = map (first incongruousity) xs
        best = fst (minimum alternatives)

    alternatives
        & filter (fst >>> (==best))
        & map snd
        & C.unlines
        & C.putStr
