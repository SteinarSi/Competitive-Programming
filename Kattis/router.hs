import           Control.Arrow ((>>>))

data Dir = U Int | R Int | L Int | D Int
    deriving Read

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map read
        >>> saw (0,0,0,0) (0,0)
        >>> putStrLn
    )

saw :: (Int,Int,Int,Int) -> (Int,Int) -> [Dir] -> String
saw (minX,maxX,minY,maxY) _ [] = show (maxX - minX + 40) <> " " <> show (maxY - minY + 40)
saw (minX,maxX,minY,maxY) (x,y) (d:ds) = saw (min minX x', max maxX x', min minY y' , max maxY y') (x',y') ds
  where
    (x',y') = case d of
        U p -> (x,y+p)
        R p -> (x+p,y)
        D p -> (x,y-p)
        L p -> (x-p,y)
