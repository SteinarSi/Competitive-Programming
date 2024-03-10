import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (elemIndex, find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    [i, _, j] <- getLine <&> (words >>> map ((`elemIndex` layouts) >>> fromJust))
    xs <- getLine
    map (translate i j) xs & putStrLn

translate :: Int -> Int -> Char -> Char
translate i j c = find ((!! i) >>> (==c)) mapping & maybe c (!!j)

layouts :: [String]
layouts = ["qwerty", "dvorak", "bjarki"]

mapping :: [String]
mapping = [
    "~~0",
    "112",
    "224",
    "338",
    "446",
    "551",
    "663",
    "775",
    "887",
    "999",
    "00=",
    "-[-",
    "=]/",
    "q'b",
    "w,j",
    "e.a",
    "rpr",
    "tyk",
    "yfi",
    "ugg",
    "icu",
    "ors",
    "plt",
    "[/.",
    "]=,",
    "aal",
    "soo",
    "dee",
    "fum",
    "gip",
    "hdd",
    "jhc",
    "ktn",
    "lnv",
    ";sq",
    "'-;",
    "z;[",
    "xq]",
    "cjy",
    "vkz",
    "bxh",
    "nbw",
    "mmf",
    ",wx",
    ".v'",
    "/z~"
    ]
