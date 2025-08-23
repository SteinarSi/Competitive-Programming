import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = do
    n:xs <- C.getContents <&> C.lines

    let (mapping, problem) = splitAt (readInt n) xs
            & map C.words
                ***
              concatMap C.words
        categories = map head mapping
            & sort
        traits = mapping
            & (concatMap (\(cat:_:ws) -> map (,[cat]) ws) >>> M.fromListWith (<>))
        score = problem
            & mapMaybe (`M.lookup` traits)
            & concatMap (map (,1))
            & M.fromListWith (+)
            & M.toList
        best = map snd score
            & maximum
        matches | null score = categories
                | otherwise  = score
                    & filter (snd >>> (==best))
                    & map fst

    C.putStr (C.unlines matches)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
