import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Maybe    (catMaybes)

main :: IO ()
main = do
    [_,_:xs] <- getContents <&> lines

    speedrun xs (Just 0, Just 0, Just 0)
        & maybe "-1" show
        & putStrLn

speedrun :: String -> (Maybe Int, Maybe Int, Maybe Int) -> Maybe Int
speedrun "" (sm,bg,fr)     = best [sm,bg,fr]
speedrun (x:xs) (sm,bg,fr) = speedrun xs $ case x of
        '.' -> (s, b, f)
        'S' -> (b, f, Nothing)
        '?' -> (s, best [s,b], best [b,f])
  where
    s = sm <&> (1+)
    b = bg <&> (2+)
    f = fr <&> (2+)

best :: [Maybe Int] -> Maybe Int
best xs = let ys = catMaybes xs
          in  if null ys
                    then Nothing
                    else Just (minimum ys)
