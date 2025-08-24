import           Data.Functor ((<&>))

main :: IO ()
main = do
    [a,[b],c',d'] <- getLine <&> words

    let c = read c'
        d = read d'
        len = length a + 2 * (c + d)

    putStr $ unlines $ concat [
            replicate c (replicate len b),
            replicate d (replicate c b <> replicate (len-2*c) ' ' <> replicate c b),
            [replicate c b <> replicate d ' ' <> a <> replicate d ' ' <> replicate c b],
            replicate d (replicate c b <> replicate (len-2*c) ' ' <> replicate c b),
            replicate c (replicate len b)
        ]
