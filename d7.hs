module D7 where

import Data.List
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.RWS
import Control.Lens
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import D7Computer
import Debug.Trace

type Programs = RWST [Int] [Int] (IM.IntMap Tape)

type Tuning = [Int]
type Output = Int

possibleTunings = permutations [0,1,2,3,4]

tape = toTape "3,8,1001,8,10,8,105,1,0,0,21,46,55,72,85,110,191,272,353,434,99999,3,9,1002,9,5,9,1001,9,2,9,102,3,9,9,101,2,9,9,102,4,9,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1002,9,2,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,1002,9,3,9,101,3,9,9,1002,9,5,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99"

test1 = toTape "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

tuneAmps :: Tape -> Tuning -> IO Int
tuneAmps tape seq = foldM f 0 seq
    where
    f signal phase = do
        (tape, outputs) <- runProgram [phase,signal] tape
        pure $ head outputs

bestTuningP1 :: Tape -> IO Int
bestTuningP1 tape = mapM (tuneAmps tape) possibleTunings >>= pure . maximum

-- Part 2

p2Tunings = permutations [5,6,7,8,9]

test2 = toTape "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

repeating :: Tuning -> [Int] -> Programs IO ()
repeating tuning inputs = do
    keys <- gets IM.keys
    out <- foldM f inputs keys

    hasRunning <- gets $ any (not . finished) . IM.elems
    if hasRunning then repeating tuning out else pure ()

    where
    f :: [Int] -> Int -> Programs IO [Int]
    f outputs i
      = zoom (ix i) $ do
          pos <- gets position
          let passTuning = if pos == 0 then [tuning !! i] else []
          passArgs (passTuning <> outputs) $ stepUntilOutput

tuneRepeating :: Tape -> Tuning -> IO Int
tuneRepeating tape tuning = do
    (tape, outputs) <- evalRWST (repeating tuning [0]) [] $ IM.fromList $ zip [0..] $ replicate 5 tape
    pure $ last outputs

bestTuningP2 :: Tape -> IO Int
bestTuningP2 tape = mapM (tuneRepeating tape) p2Tunings >>= pure . maximum
