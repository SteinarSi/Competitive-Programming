import           Data.Functor ((<&>))
import           Data.List    (transpose)

main :: IO ()
main = do
    xss <- getContents <&> lines

    let rows = length (filter ('R' `notElem`) xss)
        cols = length (filter ('R' `notElem`) (transpose xss))

    print (rows * cols)
