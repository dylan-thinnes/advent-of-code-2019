import qualified Data.Map as M
import Data.List.Split

type Reference a = M.Map Object a
type Object = String
type Orbits = Reference [Object]

type Depths = Reference Int

getOrbits :: String -> Orbits
getOrbits s = M.fromListWith (<>) $ map (\[x,y]->(x,[y])) $ map (splitOn ")") $ lines s

allDepths :: Orbits -> Depths
allDepths orbits = f 0 "COM" M.empty
    where
    f :: Int -> Object -> Depths -> Depths
    f currDepth object depths 
      = let depths' = M.insert object currDepth depths
         in case M.lookup object orbits of
              Nothing -> depths'
              Just children -> foldr (f (currDepth + 1)) depths' children

p1 = interact $ \s ->
    let orbits = getOrbits s
        depths = allDepths orbits
     in show $ sum $ M.elems depths

type Traces = Reference [Object]

allTraces :: Orbits -> Traces
allTraces orbits = f [] "COM" M.empty
    where
    f :: [Object] -> Object -> Traces -> Traces
    f currTrace object traces 
      = let traces' = M.insert object currTrace traces
         in case M.lookup object orbits of
              Nothing -> traces'
              Just children -> foldr (f (object:currTrace)) traces' children

route :: Object -> Object -> Traces -> Maybe [Object]
route a b traces = do
    at <- M.lookup a traces
    bt <- M.lookup b traces
    let common = length $ takeWhile id $ zipWith (==) (reverse at) (reverse bt)
    pure $ reverse (drop (common - 1) (reverse at)) ++ (drop common (reverse bt))

p2 = interact $ \s ->
    let orbits = getOrbits s
        traces = allTraces orbits
     in show $ (subtract 1) . length <$> route "SAN" "YOU" traces

main = p2
