import           Control.Arrow   ((>>>))
import           Data.Function   ((&))
import           Data.List       (permutations, tails)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe, listToMaybe, mapMaybe)

main :: IO ()
main = getContents >>= (
            words
        >>> map (read >>> (`mod` 12) >>> (scale!!))
        >>> permutations
        >>> mapMaybe (`M.lookup` chords)
        >>> listToMaybe
        >>> fromMaybe "neither"
        >>> putStrLn
    )

chords :: M.Map [String] String
chords = tails (scale <> scale)
    & concatMap (\(c:_:_:ds:e:_:_:g:_) -> [([c,e,g], c <> " major"), ([c,ds,g], c <> " minor")])
    & take 24
    & M.fromList

scale :: [String]
scale = ["A","A#","B","C","C#","D","D#","E","F","F#","G","G#"]
