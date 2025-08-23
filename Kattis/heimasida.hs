import           Control.Arrow   ((>>>))
import           Data.Char       (isAlphaNum, toLower)
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    getLine >>= (
                filter isAlphaNum
            >>> concatMap convert
            >>> (<> ".is")
            >>> putStrLn
        )

convert :: Char -> String
convert x = case toLower x of
    'á' -> "a"
    'ð' -> "d"
    'é' -> "e"
    'í' -> "i"
    'ó' -> "o"
    'ú' -> "u"
    'ý' -> "y"
    'þ' -> "th"
    'æ' -> "ae"
    'ö' -> "o"
    y   -> [y]
