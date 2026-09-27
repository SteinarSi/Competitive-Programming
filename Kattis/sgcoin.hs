import           Data.Char (ord)

main :: IO ()
main = do
    hash <- read <$> getLine
    hash <- next hash "steinar-quickly-adds-dummy-transaction"
    hash <- next hash "steinar-scams-everyone-mouhahahahahahahaha"
    pure ()

next :: Int -> String -> IO Int
next hash xs = putStrLn (xs <> " " <> show token) >> pure (m (hash' + token))
  where
    hash' = m (7 * foldl (\v x -> m (v*31 + ord x)) hash xs)
    token = m (1000000000 - hash')

m :: Int -> Int
m = (`mod` 1000000007)
