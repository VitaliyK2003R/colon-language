module Lang where

import Types
import qualified Data.Map as Map
import Debug.Trace (trace)

executeCmd :: Cmd -> Dict -> Stack -> IO (Result, Dict)

executeCmd (Number n) dict stack = return (Ok (n : stack), dict)

executeCmd (Word "+") dict (x : y : stack) = return (Ok ((x + y) : stack), dict)
executeCmd (Word "+") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "-") dict (x : y : stack) = return (Ok ((y - x) : stack), dict)
executeCmd (Word "-") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "*") dict (x : y : stack) = return (Ok ((x * y) : stack), dict)
executeCmd (Word "*") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "/") dict (0 : _) = return (RuntimeError DivisionByZero, dict)
executeCmd (Word "/") dict (x : y : stack) = return (Ok ((y `div` x) : stack), dict)
executeCmd (Word "/") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "dup") dict (x : stack) = return (Ok (x : x : stack), dict)
executeCmd (Word "dup") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "drop") dict (_ : stack) = return (Ok stack, dict)
executeCmd (Word "drop") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "swap") dict (x : y : stack) = return (Ok (y : x : stack), dict)
executeCmd (Word "swap") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "over") dict (x : y : stack) = return (Ok (y : x : y : stack), dict)
executeCmd (Word "over") dict _ = return (RuntimeError StackUnderflow, dict)

executeCmd (Word "error: unterminated string") dict _ = do
  putStrLn "Error: Unterminated string detected."
  return (RuntimeError StackUnderflow, dict)

executeCmd (Word word) dict stack =
  case Map.lookup word (getDefinitions dict) of
    Just (Program cmds) -> executeProgram (Program cmds) dict stack
    Nothing -> return (RuntimeError (UnknownWord word), dict)

executeCmd (Define word definition) dict stack =
  return (Ok stack, Dict $ Map.insert word definition (getDefinitions dict))

executeCmd (PrintString str) dict stack = do
  putStrLn str
  return (Ok stack, dict)

executeProgram :: Program -> Dict -> Stack -> IO (Result, Dict)
executeProgram (Program commands) dict initialStack = processCommands commands dict initialStack
  where
    processCommands :: [Cmd] -> Dict -> Stack -> IO (Result, Dict)
    processCommands [] currentDict currentStack = return (Ok currentStack, currentDict)
    processCommands (cmd : remainingCommands) currentDict currentStack = do
      trace ("Executing command: " ++ show cmd) $ return ()
      (result, newDict) <- executeCmd cmd currentDict currentStack
      case result of
        Ok newStack -> processCommands remainingCommands newDict newStack
        RuntimeError err -> return (RuntimeError err, newDict)
