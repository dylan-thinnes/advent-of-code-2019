{-# LANGUAGE TupleSections #-}

import Data.Char
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- Part 2
part2 = do
    b1s <- parseBearings <$> getLine
    b2s <- parseBearings <$> getLine
    let intersections = intersect b1s b2s
    let b1sSteps = M.restrictKeys (stepRef $ snd $ travelAll origin b1s) intersections
    let b2sSteps = M.restrictKeys (stepRef $ snd $ travelAll origin b2s) intersections
    print $ bestScore $ sumScores [b1sSteps, b2sSteps]

stepRef :: [Pos] -> M.Map Pos Int
stepRef bearings = M.fromListWith min $ zip (reverse bearings) [1..]

sumScores :: [M.Map Pos Int] -> M.Map Pos Int
sumScores = M.unionsWith (+)

bestScore :: M.Map Pos Int -> Int
bestScore = minimum . M.elems

-- Part 1
part1 = do
    b1s <- parseBearings <$> getLine
    b2s <- parseBearings <$> getLine
    let intersections = S.toList $ intersect b1s b2s
    print intersections
    print $ minimum $ map maghattan $ intersections

-- General machinery for both parts
data Bearing = Bearing Dir Int deriving (Eq, Ord)
data Dir = R | D | L | U deriving (Show, Read, Eq, Ord)

instance Show Bearing where
    show (Bearing dir length) = show dir ++ show length

instance Read Bearing where
    readsPrec _ s = do
        let dirSrc = takeWhile (not . isDigit) s
        (dir, _) <- reads dirSrc
        let rem = dropWhile (not . isDigit) s
        (length, rem') <- reads rem
        return $ (Bearing dir length, rem')

parseBearings :: String -> [Bearing]
parseBearings s = read $ "[" ++ s ++ "]"

intersect :: [Bearing] -> [Bearing] -> S.Set Pos
intersect b1 b2 = S.fromList (snd (travelAll origin b1)) `S.intersection` S.fromList (snd (travelAll origin b2))

type Pos = (Int, Int)

manhattan :: Pos -> Pos -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)
maghattan :: Pos -> Int
maghattan = manhattan (0,0)

type Cursor = (Pos, [Pos])

cursor :: Pos -> Cursor
cursor = (,[])
origin :: Cursor
origin = cursor (0,0)

travelAll :: Cursor -> [Bearing] -> Cursor
travelAll cursor bearings = foldl travel cursor bearings

travel :: Cursor -> Bearing -> Cursor
travel cursor (Bearing dir distance)
  = foldl (\c d -> editCursor c $ nudge d) cursor (distance `replicate` dir)

editCursor :: Cursor -> (Pos -> Pos) -> Cursor
editCursor (head, trace) f
  = let newHead = f head in (newHead, newHead : trace)

nudge :: Dir -> Pos -> Pos
nudge U (x,y) = (x,y+1)
nudge D (x,y) = (x,y-1)
nudge R (x,y) = (x+1,y)
nudge L (x,y) = (x-1,y)
