import           Control.Arrow     ((>>>))
import           Data.Complex      (Complex (..), imagPart, realPart)
import           Data.Functor      ((<&>))

main :: IO ()
main = do
    [a,b,c,d] <- getContents <&> (words >>> map read)

    let protagonist = a :+ b
        antagonist  = c :+ d

        result = protagonist * recip antagonist

    putStrLn (show (realPart result) <> " " <> show (imagPart result))
