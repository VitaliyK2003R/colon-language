module Lang where

import Types
import qualified Data.Map as Map
import Debug.Trace (trace)

executeCmd :: Cmd -> Dict -> (Stack, Memory) -> IO (Result, Dict)

executeCmd (Number n) dict (stack, mem) = return (Ok (n : stack, mem), dict)

executeCmd (Word "+") dict (x : y : stack, mem) = return (Ok ((x + y) : stack, mem), dict)
executeCmd (Word "+") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "-") dict (x : y : stack, mem) = return (Ok ((y - x) : stack, mem), dict)
executeCmd (Word "-") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "*") dict (x : y : stack, mem) = return (Ok ((x * y) : stack, mem), dict)
executeCmd (Word "*") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "/") dict (0 : _, _) = return (RuntimeError DivisionByZero, dict)
executeCmd (Word "/") dict (x : y : stack, _) = return (Ok ((y `div` x) : stack, emptyMemory), dict)
executeCmd (Word "/") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "dup") dict (x : stack, mem) = return (Ok (x : x : stack, mem), dict)
executeCmd (Word "dup") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "drop") dict (_ : stack, mem) = return (Ok (stack, mem), dict)
executeCmd (Word "drop") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "swap") dict (x : y : stack, mem) = return (Ok (y : x : stack, mem), dict)
executeCmd (Word "swap") dict _ = return (RuntimeError StackUnderflow, dict)
executeCmd (Word "over") dict (x : y : stack, mem) = return (Ok (y : x : y : stack, mem), dict)
executeCmd (Word "over") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word ".") dict (x : stack, mem) = do
  print x
  return (Ok (stack, mem), dict)

executeCmd (Word ".") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "cr") dict (stack, mem) = do
  putStrLn ""
  return (Ok (stack, mem), dict)

executeCmd (Word "emit") dict (x : stack, mem) = do
  putChar (toEnum x)
  return (Ok (stack, mem), dict)

executeCmd (Word "emit") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "key") dict (stack, mem) = do
  c <- getChar
  return (Ok (fromEnum c : stack, mem), dict)

executeCmd (Word ">") dict (x : y : stack, mem) = return (Ok ((if y > x then 1 else 0) : stack, mem), dict)
executeCmd (Word ">") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "<") dict (x : y : stack, mem) = return (Ok ((if y < x then 1 else 0) : stack, mem), dict)
executeCmd (Word "<") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "=") dict (x : y : stack, mem) = return (Ok ((if y == x then 1 else 0) : stack, mem), dict)
executeCmd (Word "=") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "0=") dict (x : stack, mem) = return (Ok ((if x == 0 then 1 else 0) : stack, mem), dict)
executeCmd (Word "0=") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "mod") dict (x : y : stack, mem)
  | x == 0    = return (RuntimeError DivisionByZero, dict)
  | otherwise = return (Ok ((y `mod` x) : stack, mem), dict)
executeCmd (Word "mod") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (If thenBranch elseBranch) dict (x : stack, mem)
  | x /= 0    = executeProgram thenBranch dict (stack, mem)
  | otherwise = executeProgram elseBranch dict (stack, mem)
executeCmd (If _ _) dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (PrintString str) dict (stack, mem) = do
  putStr str
  return (Ok (stack, mem), dict)

executeCmd (Define word definition) dict (stack, mem) =
  return (Ok (stack, mem), Dict $ Map.insert word definition (getDefinitions dict))

executeCmd (Word word) dict (stack, mem) =
  case Map.lookup word (getDefinitions dict) of
    Just (Program cmds) -> executeProgram (Program cmds) dict (stack, mem)
    Nothing -> return (RuntimeError (UnknownWord word), dict)

executeCmd (Word "CELLS") dict (n : stack, mem) =
  return (Ok ((n * 4) : stack, mem), dict)  -- размер ячейки 4 байта

executeCmd (Word "ALLOT") dict (n : stack, Memory mem next) =
  let newMem = foldl (\m i -> Map.insert (next + i) 0 m) mem [0..(n - 1)]
  in return (Ok (stack, Memory newMem (next + n)), dict)

executeCmd (Word "!") dict (val : addr : stack, Memory mem next) =
  if Map.member addr mem
    then return (Ok (stack, Memory (Map.insert addr val mem) next), dict)
    else return (RuntimeError MemoryAccessError, dict)

executeCmd (Word "@") dict (addr : stack, Memory mem next) =
  case Map.lookup addr mem of
    Just val -> return (Ok (val : stack, Memory mem next), dict)
    Nothing  -> return (RuntimeError MemoryAccessError, dict)

executeCmd (Word "+!") dict (val : addr : stack, Memory mem next) =
  case Map.lookup addr mem of
    Just oldVal -> return (Ok (stack, Memory (Map.insert addr (oldVal + val) mem) next), dict)
    Nothing     -> return (RuntimeError MemoryAccessError, dict)

executeCmd (Word "CREATE") dict (stack, mem) =
  case stack of
    (n : rest) ->
      let newDict = Map.insert (show n) (Program []) (getDefinitions dict)
      in return (Ok (rest, mem), Dict newDict)
    _ -> return (RuntimeError StackUnderflow, dict)


executeProgram :: Program -> Dict -> (Stack, Memory) -> IO (Result, Dict)
executeProgram (Program commands) dict (stack, mem) = processCommands commands dict (stack, mem)
  where
    processCommands [] currentDict currentState = return (Ok currentState, currentDict)
    processCommands (cmd : remainingCommands) currentDict (currentStack, currentMem) = do
      trace ("Executing command: " ++ show cmd) $ return ()
      (result, newDict) <- executeCmd cmd currentDict (currentStack, currentMem)
      case result of
        Ok newState -> processCommands remainingCommands newDict newState
        RuntimeError err -> return (RuntimeError err, newDict)
