import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [_,xs,ys,t'] <- getContents <&> lines

    reverse xs
        & map (,True)
        & (<> map (,False) ys)
        & iterate kolone
        & (!!read t')
        & map fst
        & putStrLn

kolone :: [(Char,Bool)] -> [(Char,Bool)]
kolone []  = []
kolone [x] = [x]
kolone (x@(_,a):y@(_,b):xs) | a && not b = y : x : kolone xs
                            | otherwise  = x : kolone (y:xs)
