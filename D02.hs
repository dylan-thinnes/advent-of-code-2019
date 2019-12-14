import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Data.List.Split

main = do
    tape <- toTape <$> getLine
    mapM_ print [(a, b, runProgram a b tape) | a <- [0..10], b <- [0..10]]

input :: IM.IntMap Int
input = IM.fromList . zip [0..] $ [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0]

brute :: [(Int, Int, Int)]
brute = filter (\(a,b,c) -> c == 19690720) $ [(a, b, runProgram a b input) | a <- [0..99], b <- [0..99]]

toTape :: String -> IM.IntMap Int
toTape = IM.fromList . zip [0..] . map (read :: String -> Int) . splitOn ","

runProgram :: Int -> Int -> IM.IntMap Int -> Int
runProgram a b = (!0) . runTape . IM.insert 1 a . IM.insert 2 b

runTape :: IM.IntMap Int -> IM.IntMap Int
runTape tape = execState (runOps 0) tape

data Op = Exit | Func { opFunc :: OpFunc, in1 :: Int, in2 :: Int, out :: Int }
data OpFunc = Add | Mult

runOps :: Int -> State (IM.IntMap Int) ()
runOps n = do
    maybeOp <- gets $ IM.lookup n
    case maybeOp of
      Nothing  -> pure ()
      Just 99  -> pure ()
      Just opI -> do
          let op = if opI == 1 then Add else Mult
          in1 <- gets $ (! (n + 1))
          in2 <- gets $ (! (n + 2))
          out <- gets $ (! (n + 3))
          runOp (Func op in1 in2 out)
          runOps (n + 4)

runOp :: Op -> State (IM.IntMap Int) ()
runOp (Func f in1 in2 out) = func (runOpFunc f) in1 in2 out
runOp Exit = pure ()

runOpFunc :: OpFunc -> (Int -> Int -> Int)
runOpFunc Add  = (+)
runOpFunc Mult = (*)

func :: (Int -> Int -> Int) -> Int -> Int -> Int -> State (IM.IntMap Int) ()
func f in1 in2 out = do
    a <- gets (! in1)
    b <- gets (! in2)
    modify (IM.insert out $ f a b)
