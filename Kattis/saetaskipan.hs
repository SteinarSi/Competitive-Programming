import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Maybe    (fromMaybe)

main :: IO ()
main = do
    [m,f,n] <- getContents <&> (words >>> map read)
    putStrLn (fromMaybe "O nei!" (assign '.' (m,f,n)))

assign :: Char -> (Int,Int,Int) -> Maybe String
assign _ (0,0,0) = Just []
assign p (m,f,n)
    | null options = Nothing
    | otherwise    = (c:) <$> assign c k
  where
    options = [(m,('M',(m-1,f,n))),(f,('F',(m,f-1,n))),(n,('N',(m,f,n-1)))]
        & filter (fst >>> (>0))
        & filter (snd >>> fst >>> (/=p))
    (_,(c,k)) = maximum options
