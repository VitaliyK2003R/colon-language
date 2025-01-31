module Lang where
import Types
import qualified Data.Map as Map
import Debug.Trace (trace)

modd :: Float -> Float -> Float
modd a b = a - fromIntegral (floor (a / b) :: Int) * b

executeCmd :: Cmd -> Dict -> Stack -> [Float] -> IO (Result, Dict, [Float])
executeCmd (Number n) dict stack floatStack = return (Ok (n : stack), dict, floatStack)
executeCmd (Float f) dict stack floatStack = return (Ok stack, dict, f : floatStack)
executeCmd (Word "+") dict stack floatStack
  | (fx:fy:fs) <- floatStack = return (Ok stack, dict, (fx + fy) : fs)
  | (fx:x:s) <- floatStack, (y:ys) <- stack = return (Ok ((floor (fx + fromIntegral y)) : ys), dict, s)
  | (x:fx:s) <- stack, (fy:fs) <- floatStack = return (Ok ((floor (fromIntegral x + fy)) : s), dict, fs)
  | (x:y:s) <- stack = return (Ok ((x + y) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "-") dict stack floatStack
  | (fx:fy:fs) <- floatStack = return (Ok (floor (fy - fx) : stack), dict, fs)
  | (x:y:s) <- stack = return (Ok ((y - x) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "*") dict stack floatStack
  | (x:y:s) <- stack = return (Ok ((x * y) : s), dict, floatStack)
  | (fx:fy:fs) <- floatStack = return (Ok stack, dict, (fx * fy) : fs)
  | (fx:x:s) <- floatStack, (y:ys) <- stack = return (Ok ((floor (fx * fromIntegral y)) : ys), dict, s)
  | (x:fx:s) <- stack, (fy:fs) <- floatStack = return (Ok ((floor (fromIntegral x * fy)) : s), dict, fs)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "/") dict stack floatStack
  | (fx:fy:fs) <- floatStack, fx == 0 = return (RuntimeError DivisionByZero, dict, floatStack)
  | (fx:fy:fs) <- floatStack = return (Ok (floor (fy / fx) : stack), dict, fs)
  | (0:y:s) <- stack = return (RuntimeError DivisionByZero, dict, floatStack)
  | (x:y:s) <- stack = return (Ok ((y `div` x) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "dup") dict (x : stack) floatStack = return (Ok (x : x : stack), dict, floatStack)
executeCmd (Word "dup") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "drop") dict (_ : stack) floatStack = return (Ok stack, dict, floatStack)
executeCmd (Word "drop") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "swap") dict (x : y : stack) floatStack = return (Ok (y : x : stack), dict, floatStack)
executeCmd (Word "swap") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "over") dict (x : y : stack) floatStack = return (Ok (y : x : y : stack), dict, floatStack)
executeCmd (Word "over") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word ".") dict (x : stack) floatStack = do
  print x
  return (Ok stack, dict, floatStack)
executeCmd (Word ".") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "cr") dict stack floatStack = do
  putStrLn ""
  return (Ok stack, dict, floatStack)
executeCmd (Word "emit") dict (x : stack) floatStack = do
  putChar (toEnum x)
  return (Ok stack, dict, floatStack)
executeCmd (Word "emit") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "key") dict stack floatStack = do
  c <- getChar
  return (Ok (fromEnum c : stack), dict, floatStack)
executeCmd (Word ">") dict stack floatStack
  | (fx:fy:fs) <- floatStack = return (Ok ((if fy > fx then 1 else 0) : stack), dict, fs)
  | (x:y:s) <- stack = return (Ok ((if y > x then 1 else 0) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "<") dict stack floatStack
  | (fx:fy:fs) <- floatStack = return (Ok ((if fy < fx then 1 else 0) : stack), dict, fs)
  | (x:y:s) <- stack = return (Ok ((if y < x then 1 else 0) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "=") dict stack floatStack
  | (fx:fy:fs) <- floatStack = return (Ok ((if fy == fx then 1 else 0) : stack), dict, fs)
  | (x:y:s) <- stack = return (Ok ((if y == x then 1 else 0) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "0=") dict (x : stack) floatStack = return (Ok ((if x == 0 then 1 else 0) : stack), dict, floatStack)
executeCmd (Word "0=") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "mod") dict stack floatStack
  | (fx:fy:fs) <- floatStack, fx == 0 = return (RuntimeError DivisionByZero, dict, floatStack)
  | (fx:fy:fs) <- floatStack = return (Ok stack, dict, [modd fy fx])
  | (0:_:_) <- stack = return (RuntimeError DivisionByZero, dict, floatStack)
  | (x:y:s) <- stack = return (Ok ((y `mod` x) : s), dict, floatStack)
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "f>s") dict stack floatStack
  | (fx:fs) <- floatStack = return (Ok (floor fx : stack), dict, [])
  | otherwise = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (Word "s>f") dict (x : stack) floatStack = return (Ok stack, dict, [fromIntegral x])
executeCmd (Word "s>f") dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (If thenBranch elseBranch) dict (x : stack) floatStack
  | x /= 0    = executeProgram thenBranch dict stack floatStack
  | otherwise = executeProgram elseBranch dict stack floatStack
executeCmd (If _ _) dict _ floatStack = return (RuntimeError StackUnderflow, dict, floatStack)
executeCmd (PrintString str) dict stack floatStack = do
  putStr str
  return (Ok stack, dict, floatStack)
executeCmd (Define word definition) dict stack floatStack =
  return (Ok stack, Dict $ Map.insert word definition (getDefinitions dict), floatStack)
executeCmd (Word word) dict stack floatStack =
  case Map.lookup word (getDefinitions dict) of
    Just (Program cmds) -> executeProgram (Program cmds) dict stack floatStack
    Nothing -> return (RuntimeError (UnknownWord word), dict, floatStack)

executeProgram :: Program -> Dict -> Stack -> [Float] -> IO (Result, Dict, [Float])
executeProgram (Program commands) dict initialStack initialFloatStack = processCommands commands dict initialStack initialFloatStack
  where
    processCommands :: [Cmd] -> Dict -> Stack -> [Float] -> IO (Result, Dict, [Float])
    processCommands [] currentDict currentStack currentFloatStack = return (Ok currentStack, currentDict, currentFloatStack)
    processCommands (cmd : remainingCommands) currentDict currentStack currentFloatStack = do
      trace ("Executing command: " ++ show cmd) $ return ()
      (result, newDict, newFloatStack) <- executeCmd cmd currentDict currentStack currentFloatStack
      case result of
        Ok newStack -> processCommands remainingCommands newDict newStack newFloatStack
        RuntimeError err -> return (RuntimeError err, newDict, newFloatStack)
