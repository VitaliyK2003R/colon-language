module Lang where

import Types

executeCmd :: Cmd -> Stack -> Result
executeCmd (Number n) stack = Ok (n : stack)
executeCmd (Word "+") (x : y : stack) = Ok ((x + y) : stack)
executeCmd (Word "-") (x : y : stack) = Ok ((y - x) : stack)
executeCmd (Word "*") (x : y : stack) = Ok ((x * y) : stack)
executeCmd (Word "/") (0 : _) = RuntimeError DivisionByZero
executeCmd (Word "/") (x : y : stack) = Ok ((y `div` x) : stack)
executeCmd (Word "MOD") (0 : _) = RuntimeError DivisionByZero
executeCmd (Word "MOD") (x : y : stack) = Ok ((y `mod` x) : stack)
executeCmd (Word "DUP") (x : stack) = Ok (x : x : stack)
executeCmd (Word "DROP") (_ : stack) = Ok stack
executeCmd (Word "SWAP") (x : y : stack) = Ok (y : x : stack)
executeCmd (Word "OVER") (x : y : stack) = Ok (y : x : y : stack)
executeCmd _ _ = RuntimeError StackUnderflow

executeProgram :: Program -> Stack -> Result
executeProgram (Program cmds) stack = foldl executeCommand (Ok stack) cmds
  where
    executeCommand :: Result -> Cmd -> Result
    executeCommand (RuntimeError err) _ = RuntimeError err
    executeCommand (Ok currentStack) cmd = executeCmd cmd currentStack
