import           Control.Arrow            ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List                (sort)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> drop 1 >>> sort)
        >>> combine
        >>> map C.unwords
        >>> C.unlines
        >>> C.putStr
    )

{-
This function *could* just be a onliner like this:

combine = foldr (map (:) >>> (<*>)) [[]]

But that crashes the compiler. No, really, it does.

As of the time of writing open.kattis.com uses GHC 9.4.7, and compiling this leads to an error saying that the 'impossible' happened and to please report it as a bug.
The bug is, however, fixed in later versions of GHC. But as long as open.kattis.com still uses an outdated compiler, we have to write in like this instead.
-}

combine :: [[C.ByteString]] -> [[C.ByteString]]
combine [] = [[]]
combine (xs:xss) = map (:) xs <*> combine xss
