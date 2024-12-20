module Lang where

import Types
import qualified Data.Map as Map

-- Выполнение одной команды
executeCmd :: Cmd -> Dict -> Stack -> Result
executeCmd (Number n) _ stack = Ok (n : stack)

executeCmd (Word "+") _ (x : y : stack) = Ok ((x + y) : stack)
executeCmd (Word "+") _ _ = RuntimeError StackUnderflow

executeCmd (Word "-") _ (x : y : stack) = Ok ((y - x) : stack)
executeCmd (Word "-") _ _ = RuntimeError StackUnderflow

executeCmd (Word "*") _ (x : y : stack) = Ok ((x * y) : stack)
executeCmd (Word "*") _ _ = RuntimeError StackUnderflow

executeCmd (Word "/") _ (0 : _) = RuntimeError DivisionByZero
executeCmd (Word "/") _ (x : y : stack) = Ok ((y `div` x) : stack)
executeCmd (Word "/") _ _ = RuntimeError StackUnderflow

executeCmd (Word "DUP") _ (x : stack) = Ok (x : x : stack)
executeCmd (Word "DUP") _ _ = RuntimeError StackUnderflow

executeCmd (Word "DROP") _ (_ : stack) = Ok stack
executeCmd (Word "DROP") _ _ = RuntimeError StackUnderflow

executeCmd (Word "SWAP") _ (x : y : stack) = Ok (y : x : stack)
executeCmd (Word "SWAP") _ _ = RuntimeError StackUnderflow

executeCmd (Word "OVER") _ (x : y : stack) = Ok (y : x : y : stack)
executeCmd (Word "OVER") _ _ = RuntimeError StackUnderflow

executeCmd (Word name) dict stack =
  case Map.lookup name (getDefinitions dict) of
    Just (Program cmds) -> executeProgram (Program cmds) stack dict
    Nothing             -> RuntimeError $ UnknownWord name

executeCmd _ _ _ = RuntimeError StackUnderflow -- Заглушка для других значений Cmd

-- Выполнение программы
executeProgram :: Program -> Stack -> Dict -> Result
executeProgram (Program []) stack _ = Ok stack
executeProgram (Program (cmd:cmds)) stack dict =
  case cmd of
    Word ":" -> defineWord cmds stack dict
    _        -> case executeCmd cmd dict stack of
      Ok newStack             -> executeProgram (Program cmds) newStack dict
      RuntimeError err        -> RuntimeError err
      DictUpdated newDict stk -> executeProgram (Program cmds) stk newDict

-- Определение нового слова
defineWord :: [Cmd] -> Stack -> Dict -> Result
defineWord (Word name : rest) stack dict =
  case break (== Word ";") rest of
    (body, Word ";" : _) ->
      let newDict = Dict $ Map.insert name (Program body) (getDefinitions dict)
       in DictUpdated newDict stack
    _ -> RuntimeError $ UnknownWord "Incomplete definition"
defineWord _ _ _ = RuntimeError $ UnknownWord "Invalid definition"
