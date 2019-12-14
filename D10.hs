module D10 where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.Function
import Control.Arrow

type Pos = (Int, Int)
type Asteroids = S.Set Pos

unit :: Pos -> (Pos, Int)
unit (x,y) = case abs $ gcd x y of
               0 -> ((x,y), 1)
               q -> ((x `div` q, y `div` q), q)

add :: Pos -> Pos -> Pos
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diff :: Pos -> Pos -> Pos
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mag :: Pos -> Int
mag (x,y) = x * x + y * y

toAsteroids :: String -> Asteroids
toAsteroids str = S.fromList $ do
    (row, y) <- zip (lines str) [0..]
    (cell, x) <- zip row [0..]
    guard $ cell == '#'
    pure (x,y)

test1 = toAsteroids "..#\n.#.\n"
test2 = toAsteroids ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
input = toAsteroids "#...##.####.#.......#.##..##.#.\n#.##.#..#..#...##..##.##.#.....\n#..#####.#......#..#....#.###.#\n...#.#.#...#..#.....#..#..#.#..\n.#.....##..#...#..#.#...##.....\n##.....#..........##..#......##\n.##..##.#.#....##..##.......#..\n#.##.##....###..#...##...##....\n##.#.#............##..#...##..#\n###..##.###.....#.##...####....\n...##..#...##...##..#.#..#...#.\n..#.#.##.#.#.#####.#....####.#.\n#......###.##....#...#...#...##\n.....#...#.#.#.#....#...#......\n#..#.#.#..#....#..#...#..#..##.\n#.....#..##.....#...###..#..#.#\n.....####.#..#...##..#..#..#..#\n..#.....#.#........#.#.##..####\n.#.....##..#.##.....#...###....\n###.###....#..#..#.....#####...\n#..##.##..##.#.#....#.#......#.\n.#....#.##..#.#.#.......##.....\n##.##...#...#....###.#....#....\n.....#.######.#.#..#..#.#.....#\n.#..#.##.#....#.##..#.#...##..#\n.##.###..#..#..#.###...#####.#.\n#...#...........#.....#.......#\n#....##.#.#..##...#..####...#..\n#.####......#####.....#.##..#..\n.#...#....#...##..##.#.#......#\n#..###.....##.#.......#.##...##"

surrounding :: Ord a => (Pos -> Int -> a) -> (a -> a -> a) -> Asteroids -> Pos -> M.Map Pos a
surrounding convert combine asteroids centre@(x,y) 
  = M.fromListWith combine
  $ map (\pos -> let (v,i) = unit $ diff pos centre in (v, convert (diff pos centre) i))
  $ S.toList $ S.delete centre asteroids

-- Part 1

nearest :: Asteroids -> Pos -> M.Map Pos Int
nearest = surrounding (const id) min

nearestAll :: Asteroids -> M.Map Pos (M.Map Pos Int)
nearestAll asteroids = M.fromSet (nearest asteroids) asteroids

bestPosition :: Asteroids -> (Pos, Int)
bestPosition = maximumBy (compare `on` snd) . M.toList . M.map M.size . nearestAll

-- Part 2

test3 = toAsteroids ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##"

ordered :: Asteroids -> Pos -> M.Map Pos [Pos]
ordered asteroids pos = M.map (sortBy (compare `on` mag)) 
                      $ surrounding (\a b -> pure a) (++) asteroids pos

northBearing :: Pos -> Double
northBearing (x,y) = angle + quadrant * pi
    where
        angle = atan (fromIntegral (abs y) / fromIntegral (abs x))
        quadrant | x >= 0 && y < 0 = 0
                 | x > 0 && y >= 0 = 1
                 | x <= 0 && y > 0 = 2
                 | x < 0 && y <= 0 = 3

laserOrdering asteroids pos
  = map (add pos)
  $ concat $ transpose $ M.elems 
  $ M.mapKeys northBearing 
  $ ordered asteroids pos
