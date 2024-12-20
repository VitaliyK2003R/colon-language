module Main where

import Types
import Lang
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

-- Проверка, является ли строка числом
isNumber :: String -> Bool
isNumber str =
  all (`elem` "-0123456789") str &&
  (length str > 1 || maybe True (/= '-') (listToMaybe str))

-- Парсинг команды
parseCommand :: String -> Cmd
parseCommand str
  | isNumber str = Number (read str)
  | otherwise    = Word str

-- Форматирование стека для вывода
formatStack :: Stack -> String
formatStack stack = "| " ++ intercalate " " (map show (reverse stack)) ++ " <- Top"

-- Начальный словарь
emptyDict :: Dict
emptyDict = Dict Map.empty

-- REPL (Read-Eval-Print Loop)
repl :: Stack -> Dict -> IO ()
repl stack dict = do
  putStrLn "\nВведите программу (или 'exit' для выхода):"
  input <- getLine
  if input == "exit"
    then putStrLn "Завершение работы."
    else do
      let cmds = map parseCommand (words input)
          result = executeProgram (Program cmds) stack dict
      case result of
        Ok newStack -> do
          putStrLn "> ok"
          putStrLn $ formatStack newStack
          repl newStack dict
        DictUpdated newDict newStack -> do
          putStrLn "> ok (dictionary updated)"
          putStrLn $ formatStack newStack
          repl newStack newDict
        RuntimeError err -> do
          putStrLn $ "> " ++ show err
          putStrLn $ formatStack stack
          repl stack dict

-- Главная функция
main :: IO ()
main = do
  putStrLn "Добро пожаловать в интерпретатор языка Colon!"
  repl [] emptyDict
