import qualified Data.Map as M
import Data.List.Split
import Data.Monoid

type Reference a = M.Map Object a
type Object = String
type Orbits = Reference [Object]

-- The Monoid is the yardstick of civilized code
traceMonoid :: Monoid a => (Object -> a) -> Orbits -> Reference a
traceMonoid f orbits = g mempty "COM" M.empty
    where
    g currTrace object oldRef
      = let newRef = M.insert object currTrace oldRef
            nextTrace = f object <> currTrace
         in case M.lookup object orbits of
              Nothing -> newRef
              Just children -> foldr (g nextTrace) newRef children

type Depths = Reference Int

getOrbits :: String -> Orbits
getOrbits s = M.fromListWith (<>) $ map (\[x,y]->(x,[y])) $ map (splitOn ")") $ lines s

allDepths :: Orbits -> Depths
allDepths = fmap getSum . traceMonoid (const $ Sum 1)

depthsSum :: Depths -> Int
depthsSum = sum . M.elems

p1 = interact $ show . depthsSum . allDepths . getOrbits

type Traces = Reference [Object]

allTraces :: Orbits -> Traces
allTraces = traceMonoid pure

route :: Object -> Object -> Traces -> Maybe [Object]
route a b traces = do
    at <- M.lookup a traces
    bt <- M.lookup b traces
    let common = length $ takeWhile id $ zipWith (==) (reverse at) (reverse bt)
    pure $ reverse (drop (common - 1) (reverse at)) ++ (drop common (reverse bt))

transfersBetween :: Object -> Object -> Traces -> Maybe Int
transfersBetween a b traces = fmap (subtract 1 . length) $ route a b traces

p2 = interact $ \s ->
    let orbits = getOrbits s
        traces = allTraces orbits
     in show $ (subtract 1) . length <$> route "SAN" "YOU" traces

main = p2
