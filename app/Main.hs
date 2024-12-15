module Main where

import Types
import Lang
import Data.List (intercalate)

parseCommand :: String -> Cmd
parseCommand str
  | all (`elem` "-0123456789") str && (length str > 1 || head str /= '-') = Number (read str)
  | otherwise = Word str

formatStack :: Stack -> String
formatStack stack = "| " ++ intercalate " " (map show (reverse stack)) ++ " <- Top"

repl :: Stack -> IO ()
repl stack = do
  putStrLn "\nВведите программу (или 'exit' для выхода):"
  input <- getLine
  if input == "exit"
    then putStrLn "Завершение работы."
    else do
      let cmds = map parseCommand (words input)
          result = executeProgram (Program cmds) stack
      case result of
        Ok newStack -> do
          putStrLn "> ok"
          putStrLn $ formatStack newStack
          repl newStack
        RuntimeError err -> do
          putStrLn $ "> " ++ show err
          putStrLn $ formatStack stack
          repl stack

main :: IO ()
main = do
  putStrLn "Добро пожаловать в интерпретатор языка Colon!"
  repl []
