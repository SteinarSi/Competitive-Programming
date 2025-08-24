import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y]:xss <- getContents <&> (lines >>> map (words >>> map read))

    let (xs,ys) = splitAt 6 (map (\[x2,y2] -> (x-x2)^2 + (y-y2)^2) xss)
        px = length (filter (< minimum ys) xs)
        py = length (filter (< minimum xs) ys)

    putStrLn $ case compare px py of
        LT -> "OPPONENTS\n" <> show py
        EQ -> "TIE"
        GT -> "JONNA\n" <> show px
