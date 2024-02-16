import           Control.Arrow ((>>>))
import           Data.List     (intercalate)

main :: IO ()
main = getLine >>= (
            read
        >>> ordinal
        >>> putStrLn
    )

ordinal :: Int -> String
ordinal 0 = "{}"
ordinal n = '{' : intercalate "," (map ordinal [0..n-1]) ++ "}"
