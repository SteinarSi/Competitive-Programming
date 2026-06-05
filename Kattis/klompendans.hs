import           Control.Arrow      ((>>>))
import           Control.Monad      (filterM, unless)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STUArray, inRange, newArray, range,
                                     readArray, runSTUArray, writeArray)
import           Data.Array.Unboxed ((!))
import           Data.Function      ((&))
import           Data.Functor       ((<&>))

main :: IO ()
main = do
    [n,a,b,c,d] <- getContents <&> (words >>> map read)

    let rng = (((1,1),False),((n,n),True))
        reachable = runSTUArray $ do
            seen <- newArray rng False

            let klompendans ((y,x),z) = moves
                    & map (,not z)
                    & filter (inRange rng)
                    & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
                    >>= mapM_ klompendans
                  where
                    moves
                        | z         = [(y+a,x+b),(y+a,x-b),(y-a,x+b),(y-a,x-b),(y+b,x+a),(y+b,x-a),(y-b,x+a),(y-b,x-a)]
                        | otherwise = [(y+c,x+d),(y+c,x-d),(y-c,x+d),(y-c,x-d),(y+d,x+c),(y+d,x-c),(y-d,x+c),(y-d,x-c)]

            writeArray seen ((1,1),False) True
            writeArray seen ((1,1),True) True
            klompendans ((1,1),False)
            klompendans ((1,1),True)

            pure seen

    range ((1,1),(n,n))
        & filter (\u -> reachable ! (u,False) || reachable ! (u,True))
        & length
        & print
