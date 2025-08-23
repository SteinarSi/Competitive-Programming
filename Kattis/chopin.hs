import           Control.Monad (zipWithM_)
import           Data.List     (find)
import           Data.Tuple    (swap)

main :: IO ()
main = getContents >>= (zipWithM_ (\i x -> putStrLn ("Case " ++ show i ++ ": " ++ alternate (words x))) [1..] . lines)

alternate :: [String] -> String
alternate [key, tonality] = case find ((key==) . fst) (map swap equals ++ equals) of
    Nothing    -> "UNIQUE"
    Just (_,b) -> unwords [b, tonality]

equals :: [(String, String)]
equals = [("A#", "Bb"), ("C#", "Db"), ("D#", "Eb"), ("F#", "Gb"), ("G#", "Ab")]
