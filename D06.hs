import qualified Data.Map.Strict as M
import Data.List.Split
import Data.Monoid (Sum(..))
import Data.Functor ((<&>))

type Reference a = M.Map Object a
type Object = String
type Orbits = Reference [Object]

-- The Monoid is the yardstick of civilized code
-- Converts every element to a monoid and combines it with the parent's trace monoid
-- Uses this to automatically build up a reference of traces for every object
traceMonoid :: Monoid a => (Object -> a) -> Orbits -> Reference a
traceMonoid f orbits = g mempty "COM" M.empty -- Default to an empty reference with a mempty trace
    where
    g currTrace object oldRef
      = let newRef = M.insert object currTrace oldRef
            nextTrace = f object <> currTrace
         in case M.lookup object orbits of
              Nothing -> newRef
              Just children -> foldr (g nextTrace) newRef children

-- Parse orbits out of the string format given
getOrbits :: String -> Orbits
getOrbits s = M.fromListWith (<>) $ map (\[x,y]->(x,[y])) $ map (splitOn ")") $ lines s

-- Part 1 Solution

type Depths = Reference Int

allDepths :: Orbits -> Depths
allDepths = fmap getSum . traceMonoid (const $ Sum 1)

depthsSum :: Depths -> Int
depthsSum = sum . M.elems

p1 = interact $ show . depthsSum . allDepths . getOrbits

-- Part 2 Solution

type Traces = Reference [Object]

allTraces :: Orbits -> Traces
allTraces = traceMonoid pure

route :: Object -> Object -> Traces -> Maybe [Object]
route a b traces = do
    at <- M.lookup a traces <&> reverse
    bt <- M.lookup b traces <&> reverse
    let common = length $ takeWhile id $ zipWith (==) at bt
    pure $ reverse (drop (common - 1) at) ++ drop common bt

transfersBetween :: Object -> Object -> Traces -> Maybe Int
transfersBetween a b traces = route a b traces <&> \trace -> length trace - 1

p2 = interact $ show . transfersBetween "SAN" "YOU" . allTraces . getOrbits

main = p2
