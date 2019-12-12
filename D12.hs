module D12 where

import Text.ParserCombinators.ReadP

type Pos = [Int]

combine :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
combine = zipWith

data Moon = Moon { position :: Pos, velocity :: Pos } deriving (Eq, Show)

energy :: Moon -> Int
energy moon = potential moon * kinetic moon

potential :: Moon -> Int
potential (Moon pos _) = sum $ map abs pos

kinetic :: Moon -> Int
kinetic (Moon _ vel) = sum $ map abs vel

instance Read Moon where
    readsPrec _ = readP_to_S $ do
        string "<x="
        x <- readS_to_P reads
        string ","
        skipSpaces
        string "y="
        y <- readS_to_P reads
        string ","
        skipSpaces
        string "z="
        z <- readS_to_P reads
        string ">"
        pure $ Moon [x,y,z] [0,0,0]

gravitize :: [Moon] -> [Moon]
gravitize moons = map (\moon -> foldl f moon moons) moons
    where
        f (Moon posA velA) (Moon posB velB) 
          = Moon posA $ combine (+) velA $ map signum $ combine (-) posB posA

updatePosition :: Moon -> Moon
updatePosition (Moon pos vel) = Moon (combine (+) pos vel) vel

step :: [Moon] -> [Moon]
step = map updatePosition . gravitize

test1 :: [Moon]
test1 = map read $ lines 
      $ "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"

test2 :: [Moon]
test2 = map read $ lines
      $ "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"

input :: [Moon]
input = map read $ lines 
      $ "<x=-16, y=15, z=-9>\n<x=-14, y=5, z=4>\n<x=2, y=0, z=6>\n<x=-3, y=18, z=9>"

steps :: [Moon] -> [[Moon]]
steps moons = iterate step moons

stepN :: Int -> [Moon] -> [Moon]
stepN i moons = steps moons !! i

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map energy

-- Part 2

loop :: [Moon] -> [[Moon]]
loop moons = let (x:xs) = steps moons
              in x : takeWhile (/= x) xs

slice :: Int -> Moon -> Moon
slice i (Moon pos vel) = Moon (f pos) (f vel)
    where
        f = take 1 . drop i

fullLoopLength :: [Moon] -> Int
fullLoopLength moons = foldr lcm 1 
                     $ map f [0..length (position $ moons !! 0) - 1]
    where
        f i = length $ loop $ map (slice i) moons
