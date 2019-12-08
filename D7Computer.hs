{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module D7Computer where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Data.List.Split
import Data.Function

-- Data Types

type Program = StateT Tape

data Tape = Tape { cells :: IM.IntMap Int, position :: Int, halted :: Bool, inputs :: [Int], outputs :: [Int] }
    deriving (Show)

data Op = Op { instr :: Instr, args :: [Int] }
    deriving (Show)

data Func = Add | Mult | Read | Print | Exit | JumpIfTrue | JumpIfFalse | LessThan | Equal 
    deriving (Show)

data Instr = Instr { func :: Func, imms :: [Bool] } 
    deriving (Show)

-- Tape Manipulation

toTape :: String -> Tape
toTape xs = Tape cells 0 False [] []
    where
    cells = IM.fromList . zip [0..] . map (read :: String -> Int) . splitOn "," $ xs

readTape :: Int -> Tape -> Int
readTape i (Tape { cells }) = cells ! i

writeTape :: Int -> Int -> Tape -> Tape
writeTape i v tape@(Tape { cells }) = tape { cells = IM.insert i v cells }

setPosition :: Int -> Tape -> Tape
setPosition pos tape = tape { position = pos }

halt :: Tape -> Tape
halt tape = tape { halted = True }

finished :: Tape -> Bool
finished (Tape { cells, position, halted }) = halted || position > (fst $ IM.findMax cells)

writeInput :: [Int] -> Tape -> Tape
writeInput xs tape@(Tape { inputs }) = tape { inputs = inputs <> xs }

readInput :: Tape -> (Maybe Int, Tape)
readInput tape@(Tape { inputs }) 
  = case inputs of
      (x:xs) -> (Just x, tape { inputs = xs })
      []     -> (Nothing, tape)

writeOutput :: [Int] -> Tape -> Tape
writeOutput xs tape@(Tape { outputs }) = tape { outputs = outputs <> xs }

consumeOutput :: Tape -> ([Int], Tape)
consumeOutput tape@(Tape { outputs }) = (outputs, tape { outputs = [] })

advance :: Tape -> (Int, Tape)
advance tape@(Tape { cells, position })
  = (cells ! position, tape { position = position + 1 })

advanceM = state advance

-- Func Manipulation

size :: Func -> Int
size Add   = 3
size Mult  = 3
size Read  = 1
size Print = 1
size Exit  = 0
size JumpIfTrue = 2
size JumpIfFalse = 2
size LessThan = 3
size Equal = 3

lastImm :: Func -> Maybe Bool
lastImm Add   = Just True
lastImm Mult  = Just True
lastImm Read  = Just True
lastImm Print = Nothing
lastImm Exit  = Nothing
lastImm JumpIfTrue = Nothing
lastImm JumpIfFalse = Nothing
lastImm LessThan  = Just True
lastImm Equal  = Just True

instrFromInt :: Int -> Instr
instrFromInt i = Instr func imms
    where
    imms  = case lastImm func of 
              Just x  -> init imms' ++ [x]
              Nothing -> imms'
    imms' = map (\k -> 1 == (i `div` (10 ^ k)) `mod` 10) [2..2+size func-1]
    func = case i `mod` 100 of
        99 -> Exit
        1  -> Add
        2  -> Mult
        3  -> Read
        4  -> Print
        5  -> JumpIfTrue
        6  -> JumpIfFalse
        7  -> LessThan
        8  -> Equal

-- Parsing Ops, Args

getOp :: Monad m => Program m Op
getOp = do
    instr <- instrFromInt <$> advanceM
    let argCount = instr & func & size
    args <- replicateM argCount $ advanceM
    pure $ Op instr args 

getArg :: Monad m => Int -> Bool -> Program m Int
getArg x False = gets $ readTape x
getArg x True  = pure x

-- Running Ops

-- ( Eliminate explicit recursion with CPS )
runOp :: Op -> Program IO (Program IO () -> Program IO ())
runOp (Op (Instr func imms) args) = do 
    values <- sequence $ zipWith getArg args imms
    runOpRaw func values

runOpRaw :: Func -> [Int] -> Program IO (Program IO () -> Program IO ())
runOpRaw Exit values = do
    modify halt
    pure $ const $ pure ()
runOpRaw Add  values = do
    modify $ writeTape (values !! 2) $ values !! 0 + values !! 1
    pure id
runOpRaw Mult values = do
    modify $ writeTape (values !! 2) $ values !! 0 * values !! 1
    pure id
runOpRaw Read values = do
    input <- state readInput >>= \case
        Nothing -> read <$> lift getLine
        Just x  -> pure x
    modify $ writeTape (values !! 0) input
    pure id
runOpRaw Print values = do
    modify $ writeOutput [values !! 0]
    pure id
runOpRaw JumpIfTrue values = do
    if (values !! 0) == 0
       then pure () 
       else modify $ setPosition $ values !! 1
    pure id
runOpRaw JumpIfFalse values = do
    if (values !! 0) == 0
       then modify $ setPosition $ values !! 1
       else pure ()
    pure id
runOpRaw LessThan values = do
    let areLt = fromEnum $ values !! 0 < values !! 1
    modify $ writeTape (values !! 2) $ areLt
    pure id
runOpRaw Equal values = do
    let areEq = fromEnum $ values !! 0 == values !! 1
    modify $ writeTape (values !! 2) $ areEq
    pure id

-- Run the program

step = getOp >>= runOp

stepUntilOutput :: Program IO ()
stepUntilOutput = do
    startOutput <- length <$> gets outputs
    let f = do
        op <- getOp
        cont <- runOp op
        newOutput <- length <$> gets outputs
        if startOutput == newOutput
           then cont f
           else pure ()
    f

passArgs :: Monad m => [Int] -> Program m ()
passArgs xs = modify $ writeInput xs

program :: Program IO ()
program = do
    op <- getOp
    cont <- runOp op
    cont program

runProgram :: Tape -> IO Tape
runProgram = execStateT program
