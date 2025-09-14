import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    font <- getLine
    [r1,g1,b1,r2,g2,b2] <- getContents <&> (words >>> map read)

    let score = length $ filter id [
                font == "monospace",
                r1+g1+b1 <= 25,
                r2+(255-g2)+b2 <= 35
            ]

    putStrLn $ if score >= 2
        then "L33T H4X0R"
        else "n00b"
