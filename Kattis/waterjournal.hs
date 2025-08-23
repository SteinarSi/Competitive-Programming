{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    _:a:b:xs <- fmap (map (read @Int) . words) getContents
    case (a `notElem` xs, b `notElem` xs) of
        (True , True ) -> print (-1)
        (True , False) -> print a
        (False, True)  -> print b
        (False, False) -> mapM_ print [a..b]
