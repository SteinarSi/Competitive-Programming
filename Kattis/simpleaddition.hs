import Control.Applicative (liftA2)
main = liftA2 (+) (read <$> getLine) (read <$> getLine) >>= print