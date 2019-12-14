{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module D11 where

import Prelude hiding (log)
import D11Computer
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!?),(!))
import Control.Monad.Loops
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Control.Lens as L
import Data.List.Split
import Data.Function ((&))

data Color = Black | White deriving (Show, Eq, Enum)
data Direction = U | R | D | L deriving (Show, Eq, Enum, Bounded)
type Pos = (Int, Int)
type Hull = M.Map Pos Color
data RobotState m
    = RobotState { _tile :: Pos
                 , _direction :: Direction
                 , _hull :: Hull 
                 , _brain :: Tape m
                 , _log :: [(Pos, Color, Int, Direction)]
                 }
L.makeLenses ''RobotState

class (Eq a, Enum a, Bounded a) => Cycle a where
    cycleSucc :: a -> a
    cycleSucc a | a == maxBound = minBound
                | otherwise     = succ a

    cyclePred :: a -> a
    cyclePred a | a == minBound = maxBound
                | otherwise     = pred a

instance Cycle Direction

rotateClock, rotateCounterClock :: Direction -> Direction
rotateClock = cycleSucc
rotateCounterClock = cyclePred

rotateRobot :: Int -> RobotState m -> RobotState m
rotateRobot 0 = L.over direction rotateCounterClock
rotateRobot 1 = L.over direction rotateClock

move :: Direction -> Pos -> Pos
move U (x,y) = (x,y+1)
move D (x,y) = (x,y-1)
move R (x,y) = (x+1,y)
move L (x,y) = (x-1,y)

moveRobot :: RobotState m -> RobotState m
moveRobot robot@(RobotState { _tile, _direction }) = robot { _tile = move _direction _tile }

getColor :: RobotState m -> Color
getColor robot@(RobotState { _tile, _hull }) = fromMaybe Black (_hull !? _tile)

setColor :: Color -> Pos -> Hull -> Hull
setColor color tile hull = M.insert tile color hull

setColorRobot :: Color -> RobotState m -> RobotState m
setColorRobot color robot@(RobotState { _tile, _hull }) = robot { _hull = setColor color _tile _hull }

type Robot m = Stack m (RobotState m)

toRobot :: Tape m -> RobotState m
toRobot tape = RobotState (0,0) U M.empty tape []

paint :: Monad m => Robot m ()
paint = do
    currColor <- lift $ gets getColor
    L.zoom brain $ passArgs [fromEnum $ currColor]

    color <- L.zoom brain $ do
        stepUntilOutput
        maybeToErr "No color received." =<< (listToMaybe <$> lift (state consumeOutput))
    lift $ modify (setColorRobot $ toEnum color)

    turning <- L.zoom brain $ do
        stepUntilOutput
        maybeToErr "No turn received." =<< (listToMaybe <$> lift (state consumeOutput))
    lift $ modify (rotateRobot turning)

    pos <- lift $ gets _tile
    dir <- lift $ gets _direction
    lift $ modify $ L.over log $ ((pos,toEnum color,turning,dir):)

    lift $ modify moveRobot

allTiles :: (Monad m) => Robot m ()
allTiles = whileM_ (lift $ gets $ not . finished . _brain) paint

runRobot :: Monad m
         => RobotState m -> Robot m a
         -> m (Either String a, RobotState m)
runRobot robotState robot = runStack robotState robot

drawHull :: Hull -> String
drawHull hull = unlines $ chunksOf (abs $ x2 - x1 + 1) raw
    where
        xs = S.map fst $ M.keysSet hull
        (x1,x2) = (S.findMin xs, S.findMax xs)
        ys = S.map snd $ M.keysSet hull
        (y1,y2) = (S.findMin ys, S.findMax ys)
        raw = do
            y <- reverse [y1..y2]
            x <- [x1..x2]
            pure $ case hull !? (x,y) of
                     Just White -> 'X'
                     _ -> ' '

input = toRobot $ toTape "3,8,1005,8,302,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,29,1006,0,78,2,1007,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,58,1006,0,7,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,83,2,1009,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,109,1,106,11,10,1006,0,16,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,138,2,108,0,10,1,101,14,10,1,1109,1,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,172,2,3,10,10,1006,0,49,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,201,1006,0,28,2,3,15,10,2,109,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,233,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,255,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,277,2,1107,9,10,101,1,9,9,1007,9,946,10,1005,10,15,99,109,624,104,0,104,1,21101,0,932856042280,1,21101,0,319,0,1105,1,423,21101,0,387512640296,1,21101,330,0,0,1106,0,423,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,46266346499,1,21102,1,377,0,1105,1,423,21102,1,46211836967,1,21102,1,388,0,1105,1,423,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,825460941588,1,21102,411,1,0,1106,0,423,21101,709475738388,0,1,21102,1,422,0,1105,1,423,99,109,2,21201,-1,0,1,21101,0,40,2,21102,454,1,3,21101,0,444,0,1106,0,487,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,449,450,465,4,0,1001,449,1,449,108,4,449,10,1006,10,481,1102,1,0,449,109,-2,2106,0,0,0,109,4,2102,1,-1,486,1207,-3,0,10,1006,10,504,21101,0,0,-3,22101,0,-3,1,21201,-2,0,2,21102,1,1,3,21102,1,523,0,1105,1,528,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,551,2207,-4,-2,10,1006,10,551,22101,0,-4,-4,1105,1,619,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,570,0,0,1106,0,528,22102,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,589,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,611,21201,-1,0,1,21101,611,0,0,106,0,486,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0" undefined

p1 :: IO ()
p1 = do
    let startState = input
    (output, state) <- runRobot startState allTiles
    print $ M.size $ _hull state

p2 :: IO ()
p2 = do
    let startState = input & L.over hull (setColor White (0,0))
    (output, state) <- runRobot startState allTiles
    putStrLn $ drawHull $ _hull state
