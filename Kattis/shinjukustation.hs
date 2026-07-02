import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n:_:rest <- getContents <&> words

    let (arrivals, departures) = init rest
            & map (\(h1:h2:_:m1:m2:_:s1:s2:_) -> read [s1,s2] + 60 * read [m1,m2] + 60*60 * read [h1,h2])
            & splitAt (read n)
        t = read (last rest)
        options = do
            arr <- arrivals
            dep <- departures
            guard (arr + t <= dep)
            pure (dep - arr)

    print $ if null options
        then -1
        else minimum options
