import           Control.Arrow         (second, (***), (>>>))
import           Control.Monad         (filterM, guard)
import           Control.Monad.ST      (ST, runST)
import           Data.Bits             (popCount, shift, (.&.))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Ix               (range)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: Int -> String
solve game = runST $ do
    seen <- newSTRef S.empty
    best <- play seen game
    pure (show best <> " " <> show (popCount game - best))
  where
    play :: STRef s S.IntSet -> Int -> ST s Int
    play seen u = positions
            & concatMap moves
            & filterM (\v -> readSTRef seen >>= (S.member v >>> bool (modifySTRef seen (S.insert v) >> pure True) (pure False)))
            >>= mapM (play seen) <&> ((popCount u:) >>> minimum)
      where
        has :: Int -> Bool
        has = (u .&.) >>> (>0)

        moves :: Int -> [Int]
        moves i = do
            guard (has i)
            (dir,no) <- directions
            guard (no .&. i == 0)
            guard (has (dir i))
            guard (not (has (dir (dir i))))
            pure (u - i - dir i + dir (dir i))

parse :: [C.ByteString] -> [Int]
parse [] = []
parse xss = drop 1 xss
    & splitAt 5
    & parseGame *** parse
    & uncurry (:)
  where
    parseGame :: [C.ByteString] -> Int
    parseGame = concatMap C.unpack
        >>> zipWith (bool 0 >>> (('o'==) >>>)) positions
        >>> sum

positions :: [Int]
positions = map (shift 1) [0..44]

directions :: [(Int -> Int, Int)]
directions = [
    ((`shift` (-9)),[((1,4),(2,6)),((2,1),(3,3)),((2,7),(3,9))]),
    ((`shift` 9),[((3,1),(4,3)),((4,4),(5,6)),((3,7),(4,9))]),
    ((`shift` (-1)),[((1,4),(1,5)),((2,1),(4,2)),((5,4),(5,5))]),
    ((`shift` 1),[((1,5),(1,6)),((5,5),(5,6)),((2,8),(4,9))])
    ]
    & map (second (concatMap (range >>> map at) >>> sum))
  where
    at :: (Int,Int) -> Int
    at (r,c) = 1 `shift` (9*(r-1)+c-1)
