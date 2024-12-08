module Year.Year2023.Day24(Day24(Day24), testZ3) where

import           Control.Monad    (forM_, join)
import           Data.Maybe
import qualified Data.Traversable as T
import           Z3.Monad

import           Meta             (AoC (..))
import           System.IO.Unsafe (unsafePerformIO)
import           Utility.Misc

data Day24 = Day24
instance AoC Day24 [Hail] Integer where
    date _ = (24,2023)
    parse _ = map parseHail . lines
    part1 _ = allIntersections testArea
    part2 _ = sum . fromJust . unsafePerformIO . evalZ3 . throwSnowball
    testAnswerPart1 _ = 0 -- The test should use a different area. (7, 27) instead of testArea.
    testAnswerPart2 _ = 47

type Vector = (Double, Double, Double)
type Hail = (Vector, Vector)

testArea :: (Double, Double)
testArea = (200000000000000,400000000000000)

parseHail :: String -> Hail
parseHail s = ((x,y,z),(dx,dy,dz))
    where [x,y,z,dx,dy,dz] = extractIntegers s

allIntersections :: (Double,Double) -> [Hail] -> Integer
allIntersections _   [] = 0
allIntersections rng (hail:hs) = length' (filter id (map (intersect hail) hs)) + allIntersections rng hs
    where
        intersect :: Hail -> Hail -> Bool
        intersect ((x,y,_),(dx,dy,_)) ((a,b,_),(da,db,_)) = and [m1 /= m2, inbetween xCross rng, inbetween yCross rng, t >= 0, s >= 0]
            where
                m1 = dy / dx
                m2 = db / da
                b1 = y - m1 * x
                b2 = b - m2 * a
                xCross  = (b2-b1) / (m1-m2)
                yCross  = m1 * xCross + b1
                t       = (xCross - x) / dx
                s       = (xCross - a) / da

throwSnowball :: [Hail] -> Z3 (Maybe [Integer])
throwSnowball hails = do
    [x, y, z, dx, dy, dz] <- mapM mkFreshRealVar ["x", "y", "z", "dx", "dy", "dz"]
    zero <- mkRealNum 0 -- Real?
    constants <- mapM (\((a,b,c),(da,db,dc)) -> mapM mkRealNum [a,b,c,da,db,dc]) (take 3 hails)
    forM_ (zip [1::Int ..] constants) $ \(i, [a,b,c,da,db,dc]) -> do

        t <- mkFreshRealVar (show i)
        -- assert =<< mkGe t zero

        -- dxt <- mul dx t
        xdxt <- add x =<< mul dx t
        -- dat <- mul da t
        adat <- add a =<< mul da t
        -- assert =<< mkEq xdxt adat

        -- dyt <- mkMul [dy, t]
        ydyt <- add y =<< mul dy t
        -- dbt <- mkMul [db, t]
        bdbt <- add b =<< mul db t
        -- assert =<< mkEq ydyt bdbt

        -- dzt <- mkMul [dz, t]
        zdzt <- add z =<< add dz t
        -- zdzt <- mkAdd [z, dzt]
        dct <- mkMul [dc, t]
        -- cdct <- add c =<< mul dc t
        cdct <- mkAdd [c, dct]
        -- assert =<< mkEq zdzt cdct

        assert =<< mkAnd =<< T.sequence [
                mkGe t zero,
                mkEq xdxt adat,
                mkEq ydyt bdbt,
                mkEq zdzt cdct
            ]

    evalss <- withModel $ \m -> catMaybes <$> mapM (evalInt m) [x,y,z]

    trace' evalss pure (Just [])

    fmap snd $ withModel $ \m -> catMaybes <$> mapM (evalInt m) [x,y,z]

    where add a b = mkAdd [a, b]
          mul a b = mkMul [a, b]


script :: Z3 (Maybe [Integer])
script = do
    q1 <- mkFreshIntVar "q1"
    q2 <- mkFreshIntVar "q2"
    q3 <- mkFreshIntVar "q3"
    q4 <- mkFreshIntVar "q4"
    _1 <- mkInteger 1
    _4 <- mkInteger 4
    assert =<< mkAnd =<< T.sequence
        [ mkLe _1 q1, mkLe q1 _4  -- 1 <= q1 <= 4
        , mkLe _1 q2, mkLe q2 _4
        , mkLe _1 q3, mkLe q3 _4
        , mkLe _1 q4, mkLe q4 _4
        ]
    assert =<< mkDistinct [q1,q2,q3,q4]
    assert =<< mkNot =<< mkOr =<< T.sequence
        [ diagonal 1 q1 q2  -- diagonal line of attack between q1 and q2
        , diagonal 2 q1 q3
        , diagonal 3 q1 q4
        , diagonal 1 q2 q3
        , diagonal 2 q2 q4
        , diagonal 1 q3 q4
        ]
    -- check and get solution
    fmap snd $ withModel $ \m -> catMaybes <$> mapM (evalInt m) [q1,q2,q3,q4]
    where mkAbs x = do
              _0 <- mkInteger 0
              join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
          diagonal d c c' = join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> mkInteger d

testZ3 :: IO ()
testZ3 = evalZ3 script >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStr "Solution: " >> print sol

