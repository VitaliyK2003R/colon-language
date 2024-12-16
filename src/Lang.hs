module Lang where

import Types

executeCmd :: Cmd -> Stack -> Result
executeCmd (Number n) stack = Ok (n : stack)

executeCmd (Word "+") (x : y : stack) = Ok ((x + y) : stack)
executeCmd (Word "+") _ = RuntimeError StackUnderflow

executeCmd (Word "-") (x : y : stack) = Ok ((y - x) : stack)
executeCmd (Word "-") _ = RuntimeError StackUnderflow

executeCmd (Word "*") (x : y : stack) = Ok ((x * y) : stack)
executeCmd (Word "*") _ = RuntimeError StackUnderflow

executeCmd (Word "/") (0 : _) = RuntimeError DivisionByZero
executeCmd (Word "/") (x : y : stack) = Ok ((y `div` x) : stack)
executeCmd (Word "/") _ = RuntimeError StackUnderflow

executeCmd (Word "MOD") (0 : _) = RuntimeError DivisionByZero
executeCmd (Word "MOD") (x : y : stack) = Ok ((y `mod` x) : stack)
executeCmd (Word "MOD") _ = RuntimeError StackUnderflow

executeCmd (Word "DUP") (x : stack) = Ok (x : x : stack)
executeCmd (Word "DUP") _ = RuntimeError StackUnderflow

executeCmd (Word "DROP") (_ : stack) = Ok stack
executeCmd (Word "DROP") _ = RuntimeError StackUnderflow

executeCmd (Word "SWAP") (x : y : stack) = Ok (y : x : stack)
executeCmd (Word "SWAP") _ = RuntimeError StackUnderflow

executeCmd (Word "OVER") (x : y : stack) = Ok (y : x : y : stack)
executeCmd (Word "OVER") _ = RuntimeError StackUnderflow

executeCmd (Word "=") (x : y : stack) = Ok ((if y == x then -1 else 0) : stack)
executeCmd (Word "=") _ = RuntimeError StackUnderflow

executeCmd (Word "<") (x : y : stack) = Ok ((if y < x then -1 else 0) : stack)
executeCmd (Word "<") _ = RuntimeError StackUnderflow

executeCmd (Word ">") (x : y : stack) = Ok ((if y > x then -1 else 0) : stack)
executeCmd (Word ">") _ = RuntimeError StackUnderflow

executeCmd (Word "IF") (cond : stack)
  | cond /= 0 = Ok stack
executeCmd (Word "IF") _ = RuntimeError StackUnderflow

executeCmd (Word "THEN") stack = Ok stack

executeCmd (Word "DO") (upper : lower : stack) = Ok (lower : upper : lower : stack)
executeCmd (Word "DO") _ = RuntimeError StackUnderflow

executeCmd (Word "I") (i : upper : lower : stack) = Ok (i : i : upper : lower : stack)
executeCmd (Word "I") _ = RuntimeError StackUnderflow

executeCmd (Word "LOOP") (i : upper : lower : stack)
  | i + 1 < upper = Ok (i + 1 : upper : lower : stack)
  | otherwise     = Ok stack
executeCmd (Word "LOOP") _ = RuntimeError StackUnderflow

executeCmd _ _ = RuntimeError StackUnderflow


executeProgram :: Program -> Stack -> Result
executeProgram (Program commands) initialStack = processCommands commands initialStack
  where
    processCommands :: [Cmd] -> Stack -> Result
    processCommands [] currentStack = Ok currentStack
    processCommands (cmd : remainingCommands) currentStack =
      case executeCmd cmd currentStack of
        Ok newStack -> case cmd of
          Word "DO" -> processLoop remainingCommands newStack
          _         -> processCommands remainingCommands newStack
        RuntimeError err -> RuntimeError err

    processLoop :: [Cmd] -> Stack -> Result
    processLoop _ [] = RuntimeError StackUnderflow
    processLoop _ [_] = RuntimeError StackUnderflow
    processLoop loopCommands (lower : upper : rest)
      | lower >= upper = Ok rest
      | otherwise = case processCommands loopCommands (lower : upper : lower : rest) of
          Ok _ -> processLoop loopCommands (lower + 1 : upper : rest)
          RuntimeError err -> RuntimeError err
