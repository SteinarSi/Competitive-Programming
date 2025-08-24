import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    ur:lr:h:b:_ <- getContents <&> (words >>> map read)

    let volume = 3.14159 * h * (ur^^2 + ur*lr + lr^^2) / 3
        birds = floor (volume / b)

    putStrLn (show birds <> " birds")
