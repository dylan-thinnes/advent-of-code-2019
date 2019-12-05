{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.RWS.Lazy
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Data.List.Split
import Data.Function

data Tape = Tape
    { cells :: IM.IntMap Int
    , position :: Int
    } deriving (Show)

type Program = RWST [String] [String] Tape

data Op = Op { instr :: Instr, args :: [Int] } deriving (Show)
data Func = Add | Mult | Read | Print | Exit deriving (Show)
data Instr = Instr { func :: Func, imms :: [Bool] } deriving (Show)

toTape :: String -> Tape
toTape xs = Tape cells 0
    where
    cells = IM.fromList . zip [0..] . map (read :: String -> Int) . splitOn "," $ xs

readTape :: Int -> Tape -> Int
readTape i (Tape cells _) = cells ! i

writeTape :: Int -> Int -> Tape -> Tape
writeTape i v (Tape cells pos) = Tape (IM.insert i v cells) pos

finished :: Tape -> Bool
finished (Tape cells position) = position > (fst $ IM.findMax cells)

size :: Func -> Int
size Add   = 3
size Mult  = 3
size Read  = 1
size Print = 1
size Exit  = 0

writes :: Func -> Bool
writes Add   = True
writes Mult  = True
writes Read  = True
writes Print = False
writes Exit  = False

instrFromInt :: Int -> Instr
instrFromInt i = Instr func imms
    where
    imms  = if writes func then init imms' ++ [True] else imms'
    imms' = map (\k -> 1 == (i `div` (10 ^ k)) `mod` 10) [2..2+size func-1]
    func = case i `mod` 100 of
        99 -> Exit
        1  -> Add
        2  -> Mult
        3  -> Read
        4  -> Print

advance :: Tape -> (Int, Tape)
advance (Tape cells position)
  = (cells ! position, Tape cells $ position + 1)

advanceM = state advance

getOp :: Monad m => Program m Op
getOp = do
    instr <- instrFromInt <$> advanceM
    let argCount = instr & func & size
    args <- replicateM argCount $ advanceM
    pure $ Op instr args 

getArg :: Monad m => Int -> Bool -> Program m Int
getArg x False = gets $ readTape x
getArg x True  = pure x

-- Eliminate explicit recursion with CPS
runOp :: Op -> Program IO (Program IO () -> Program IO ())
runOp (Op (Instr func imms) args) = do
    values <- sequence $ zipWith getArg args imms
    case func of
        Exit  -> pure $ const $ pure ()
        Add   -> do
            modify $ writeTape (values !! 2) $ values !! 0 + values !! 1
            pure id
        Mult  -> do
            modify $ writeTape (values !! 2) $ values !! 0 * values !! 1
            pure id
        Read  -> do
            input <- lift getLine
            modify $ writeTape (values !! 0) $ read input
            pure id
        Print -> do
            --tell [show $ values !! 0]
            lift $ putStrLn $ show $ values !! 0
            pure id

-- Run the program
program :: Program IO ()
program = do
    op <- getOp
    cont <- runOp op
    cont program

runProgram :: [String] -> Tape -> IO (Tape, [String])
runProgram = execRWST program

main = do
    tape <- toTape <$> getLine
    --inputs <- sequence $ repeat getLine
    print =<< runProgram [] tape
    pure ()
