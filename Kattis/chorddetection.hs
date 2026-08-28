import           Control.Arrow   ((>>>))
import           Data.Function   ((&))
import           Data.List       (elemIndex, sortOn, tails)
import qualified Data.Map.Strict as M

main :: IO ()
main = getContents >>= (
            words
        >>> sortOn (`elemIndex` scale)
        >>> flip (M.findWithDefault "not a chord") chords
        >>> putStrLn
    )

chords :: M.Map [String] String
chords = tails (scale <> scale)
    & map (\(c:_:_:_:e:_:_:g:_) -> (sortOn (`elemIndex` scale) [c,e,g], c <> " major"))
    & take 12
    & M.fromList

scale :: [String]
scale = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
