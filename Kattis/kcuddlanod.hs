import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            twoFive
        >>> words
        >>> map (reverse >>> (read::String->Int))
        >>> flip zip [1..]
        >>> maximum
        >>> snd
        >>> print
    )

twoFive :: String -> String
twoFive ""       = ""
twoFive ('2':xs) = '5' : twoFive xs
twoFive ('5':xs) = '2' : twoFive xs
twoFive (x:xs)   = x : twoFive xs
