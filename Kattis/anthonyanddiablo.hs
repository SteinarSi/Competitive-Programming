main :: IO ()
main = do
    a:n:_ <- fmap (map read .words) getContents
    if n*n/(4*pi) >= a
        then putStrLn "Diablo is happy!"
        else putStrLn "Need more materials!"
