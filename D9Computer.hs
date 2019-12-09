{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module D9Computer where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Loops
import qualified Data.IntMap as IM
import Data.IntMap ((!), (!?))
import Data.List.Split
import Data.Function
import Data.Maybe

-- Data Types

type Stack m s a = ExceptT String (StateT s m) a
type Program m a = Stack m Tape a

data Tape = Tape { cells :: IM.IntMap Int
                 , position :: Int
                 , halted :: Bool
                 , inputs :: [Int]
                 , outputs :: [Int] 
                 , relBase :: Int
                 }
    deriving (Show)

data Op = Op { instr :: Instr, args :: [Int] }
    deriving (Show)

data Func = Add | Mult | Read | Print | Exit | JumpIfTrue | JumpIfFalse | LessThan | Equal 
          | AdjustRel
    deriving (Show)

data Instr = Instr { func :: Func, modes :: [Mode] } 
    deriving (Show)

data Mode = Position | Immediate | Relative
    deriving (Eq, Show, Enum)

-- Tape Manipulation

toTape :: String -> Tape
toTape xs = Tape cells 0 False [] [] 0
    where
    cells = IM.fromList . zip [0..] . map (read :: String -> Int) . splitOn "," $ xs

readTape :: Int -> Tape -> Maybe Int
readTape i (Tape { cells }) = cells !? i

readInfiniteTape :: Int -> Tape -> Maybe Int
readInfiniteTape i tape = case readTape i tape of
                            Nothing -> if i >= 0 then Just 0 else Nothing
                            x       -> x

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

adjustRelBase :: Int -> Tape -> Tape
adjustRelBase i tape@(Tape { relBase }) = tape { relBase = relBase + i }

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
size AdjustRel = 1

lastImm :: Func -> Maybe Mode
lastImm Add   = Just Immediate
lastImm Mult  = Just Immediate
lastImm Read  = Just Immediate
lastImm Print = Nothing
lastImm Exit  = Nothing
lastImm JumpIfTrue = Nothing
lastImm JumpIfFalse = Nothing
lastImm LessThan  = Just Immediate
lastImm Equal  = Just Immediate
lastImm AdjustRel = Nothing

outFlags :: Func -> [Bool]
outFlags x = case x of
         Add         -> replicate (size x - 1) False ++ [True]
         Mult        -> replicate (size x - 1) False ++ [True]
         Read        -> replicate (size x - 1) False ++ [True]
         LessThan    -> replicate (size x - 1) False ++ [True]
         Equal       -> replicate (size x - 1) False ++ [True]
         Print       -> replicate (size x) False
         Exit        -> replicate (size x) False
         JumpIfTrue  -> replicate (size x) False
         JumpIfFalse -> replicate (size x) False
         AdjustRel   -> replicate (size x) False

instrFromInt :: Int -> Instr
instrFromInt i = Instr func modes
    where
    modes = map (\k -> toEnum $ (i `div` (10 ^ k)) `mod` 10) [2..2+size func-1]
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
        9  -> AdjustRel

-- Error Wrangling

maybeToErr :: Monad m => String -> Maybe a -> Program m a 
maybeToErr _ (Just x) = pure x
maybeToErr s Nothing  = throwE s

-- Parsing Ops, Args

getOp :: Monad m => Program m Op
getOp = lift $ do
    instr <- instrFromInt <$> advanceM
    let argCount = instr & func & size
    args <- replicateM argCount $ advanceM
    pure $ Op instr args

-- Handle arguments, both for reads and writes to the tape

getArg :: Monad m => (Int, Bool) -> Mode -> Program m Int
getArg (x, write) mode = (if write then getArgWrite else getArgRead) x mode
    where
    getArgRead x Position
      = (lift $ gets $ readInfiniteTape x)
        >>= maybeToErr ("No read at position " ++ show x)
    getArgRead x Immediate = lift $ pure x
    getArgRead x Relative = do
        base <- lift $ gets relBase
        getArgRead (base + x) Position

    getArgWrite x Position = lift $ pure x
    getArgWrite x Immediate = lift $ pure x
    getArgWrite x Relative = do
        base <- lift $ gets relBase
        pure (base + x)

-- Running Ops

runOp :: Op -> Program IO ()
runOp (Op (Instr func modes) args) = do 
    values <- sequence $ zipWith getArg (zip args $ outFlags func) modes
    runOpRaw func values

runOpRaw :: Func -> [Int] -> Program IO ()
runOpRaw Exit values = do
    lift $ modify halt
    throwE "Halted normally."
runOpRaw Add  values = lift $ do
    modify $ writeTape (values !! 2) $ values !! 0 + values !! 1
runOpRaw Mult values = lift $ do
    modify $ writeTape (values !! 2) $ values !! 0 * values !! 1
runOpRaw Read values = lift $ do
    input <- state readInput >>= \case
        Nothing -> read <$> lift getLine
        Just x  -> pure x
    modify $ writeTape (values !! 0) input
runOpRaw Print values = lift $ do
    modify $ writeOutput [values !! 0]
runOpRaw JumpIfTrue values = lift $ do
    if (values !! 0) == 0
       then pure () 
       else modify $ setPosition $ values !! 1
runOpRaw JumpIfFalse values = lift $ do
    if (values !! 0) == 0
       then modify $ setPosition $ values !! 1
       else pure ()
runOpRaw LessThan values = lift $ do
    let areLt = fromEnum $ values !! 0 < values !! 1
    modify $ writeTape (values !! 2) $ areLt
runOpRaw Equal values = lift $ do
    let areEq = fromEnum $ values !! 0 == values !! 1
    modify $ writeTape (values !! 2) $ areEq
runOpRaw AdjustRel values = lift $ do
    modify $ adjustRelBase $ values !! 0

-- Run the program

step = getOp >>= runOp

stepUntilOutput :: Program IO ()
stepUntilOutput = do
    startOutput <- lift $ length <$> gets outputs
    let f = do
        step
        newOutput <- lift $ length <$> gets outputs
        if startOutput == newOutput
           then f
           else pure ()
    f

passArgs :: Monad m => [Int] -> Program m ()
passArgs xs = lift $ modify $ writeInput xs

program :: Program IO ()
program = whileM_ (lift $ gets $ not . finished) step

runStack :: s -> Stack IO s a -> IO (Either String a, s)
runStack state stack = flip runStateT state $ runExceptT stack

runProgram :: Tape -> Program IO () -> IO Tape
runProgram tape program = flip execStateT tape $ runExceptT program
