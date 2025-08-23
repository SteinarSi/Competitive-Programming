-- This entire solution is just copypaste from the generalized problem Mate In One.

import           Control.Applicative ((<|>))
import           Control.Arrow       ((>>>))
import           Data.Array          (Array, inRange, listArray, range, (!),
                                      (//))
import           Data.Bool           (bool)
import           Data.Char           (chr, isSpace, isUpper, ord, toUpper)
import           Data.Function       ((&))
import           Data.Functor        ((<&>))
import           Data.List           (find)
import           Data.Maybe          (isNothing)

data Color = White | Black deriving (Eq)
data Kind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)
type Piece = (Color, Kind)
type Board = Array Pos (Maybe Piece)
type Pos = (Int,Int)
data Move = Move {
    from :: Pos,
    to   :: Pos,
    prom :: Maybe Piece
}

main :: IO ()
main = do
    bård <- getContents <&> (filter (isSpace >>> not) >>> map parse >>> listArray rng)
    let possible = any (makeMove bård >>> checkmate) (legalMoves White bård)
    putStrLn $ if any (makeMove bård >>> checkmate) (legalMoves White bård)
        then "Yes"
        else "No"

checkmate :: Board -> Bool
checkmate bård = null (legalMoves Black bård) && any (to >>> (bård !) >>> (== Just (Black,King))) (pseudoMoves White bård)

legalMoves :: Color -> Board -> [Move]
legalMoves color bård = filter legal (pseudoMoves color bård)
  where
    legal :: Move -> Bool
    legal move = let bård' = makeMove bård move
                 in  all (to >>> (bård' !) >>> (/= Just (color,King))) (pseudoMoves (op color) bård')

makeMove :: Board -> Move -> Board
makeMove bård Move {from, to, prom} = bård // [(from,Nothing),(to,prom <|> bård!from)]

pseudoMoves :: Color -> Board -> [Move]
pseudoMoves color bård = concatMap pieceMoves (range rng)
  where
    pieceMoves :: Pos -> [Move]
    pieceMoves from@(y,x) = case bård ! from of
        Nothing -> []
        Just p@(c,k) | c /= color -> []
                     | k == Pawn  -> directions p
                          & filter legalPawn
                          & concatMap ((from+++) >>> \to@(y,_) -> map (Move from to) (bool [Nothing] [Just (color,Queen),Just (color,Knight)] (y `elem` [1,8])))
                     | k `elem` [Bishop,Rook,Queen] -> concatMap (\delta -> run delta (from+++delta)) (directions p)
                     | otherwise  -> directions p
                          & map (from+++)
                          & filter (inRange rng)
                          & filter ((bård!) >>> (<&> fst) >>> (/=Just color))
                          & map (\to -> Move {from=from,to=to,prom=Nothing})
      where
        legalPawn :: Pos -> Bool
        legalPawn d@(dy,dx) | not (inRange rng to) = False
                            | abs dx == 1 = (bård!to <&> fst) == Just (op color)
                            | abs dy == 2 = isNothing (bård ! to) && isNothing (bård ! (from+++(dy`div`2,dx))) && (color==White && y==7 || color == Black && y == 2)
                            | otherwise   = isNothing (bård ! to)
            where to = from +++ d

        run :: Pos -> Pos -> [Move]
        run delta to | not (inRange rng to) = []
                     | otherwise = case bård ! to of
                          Nothing -> Move from to Nothing : run delta (delta+++to)
                          Just (c,_) | c == color -> []
                                     | otherwise  -> [Move from to Nothing]

directions :: Piece -> [Pos]
directions (White,Pawn) = [(-1,0),(-2,0),(-1,-1),(-1,1)]
directions (Black,Pawn) = [( 1,0),( 2,0),( 1,-1),( 1,1)]
directions (_    ,kind) = case kind of
    Knight -> [(-1,-2),(-2,-1),(-2,1),(-1,2),(1,2),(2,1),(2,-1),(1,-2)]
    Bishop -> [(-1,-1),(-1,1),(1,1),(1,-1)]
    Rook   -> [(-1,0),(0,1),(1,0),(0,-1)]
    Queen  -> [(-1,-1),(-1,1),(1,1),(1,-1),(-1,0),(0,1),(1,0),(0,-1)]
    King   -> [(-1,-1),(-1,1),(1,1),(1,-1),(-1,0),(0,1),(1,0),(0,-1)]

parse :: Char -> Maybe Piece
parse '.' = Nothing
parse  x  = Just (c,k)
  where
    c | isUpper x = White
      | otherwise = Black
    k = case toUpper x of
        'P' -> Pawn
        'N' -> Knight
        'B' -> Bishop
        'R' -> Rook
        'Q' -> Queen
        'K' -> King

format :: Move -> String
format Move {from,to} = f from <> f to
    where f (y,x) = chr (ord 'a' + x - 1) : show (9-y)

op :: Color -> Color
op White = Black
op Black = White

rng :: (Pos,Pos)
rng = ((1,1),(8,8))

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(+++) (x,y) (a,b) = (x+a, y+b)
