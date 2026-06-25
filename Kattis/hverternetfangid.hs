import           Control.Arrow (first, (>>>))

main :: IO ()
main = getLine >>= (
            words
        >>> last
        >>> span (/='@')
        >>> first (takeWhile (/='+'))
        >>> uncurry (<>)
        >>> putStrLn
    )
