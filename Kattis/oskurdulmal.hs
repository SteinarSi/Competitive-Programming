import           Data.Bool       (bool)
import           Data.Functor    ((<&>))
import qualified Data.Map.Strict as M
import           Data.Tuple      (swap)

main :: IO ()
main = do
    [de,xs] <- getContents <&> lines
    putStrLn (code (bool decode encode (de=="E")) xs)

code :: M.Map String String -> String -> String
code _ ""       = ""
code m [x]      = M.findWithDefault [x] [x] m
code m (x:y:xs) = case (M.lookup [x] m, M.lookup [x,y] m) of
    (_, Just z) -> z <> code m xs
    (Just z, _) -> z <> code m (y:xs)
    _           -> x :  code m (y:xs)

encode :: M.Map String String
encode = M.fromList mapping

decode :: M.Map String String
decode = M.fromList (map swap mapping)

mapping :: [(String,String)]
mapping = [
    (" ", " "),
    ("A","A"),
    ("B","Ȧ"),
    ("C","A̧"),
    ("D","A̱"),
    ("E","Á"),
    ("F","A̮"),
    ("G","A̋"),
    ("H","A̰"),
    ("I","Ả"),
    ("J","A̓"),
    ("K","Ạ"),
    ("L","Ă"),
    ("M","Ǎ"),
    ("N","Â"),
    ("O","Å"),
    ("P","A̯"),
    ("Q","A̤"),
    ("R","Ȃ"),
    ("S","Ã"),
    ("T","Ā"),
    ("U","Ä"),
    ("V","À"),
    ("W","Ȁ"),
    ("X","A̽"),
    ("Y","A̦"),
    ("Z","Ⱥ")
    ]
