module Main where

import Types
import Lang
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Char (toLower)
import Debug.Trace (trace)

tokenize :: String -> [String]
tokenize "" = []
tokenize ('.':'"':rest) =
  let (str, rest') = span (/= '"') rest
  in case rest' of
       [] -> trace ("Unterminated string detected: " ++ str) [".\"" ++ str]
       ('"':remaining) ->
         let completeString = ".\"" ++ str ++ "\""
         in trace ("Completed string token: " ++ completeString) (completeString : tokenize remaining)
       _ -> error "Unexpected tokenization state"
tokenize input =
  let (token, rest) = break (`elem` " \t\n") input
      trimmedToken = dropWhile (`elem` " \t\n") token
  in if null trimmedToken
        then tokenize (dropWhile (`elem` " \t\n") rest)
        else trace ("Token detected: " ++ trimmedToken) (trimmedToken : tokenize (dropWhile (`elem` " \t\n") rest))

parseCommand :: String -> [Cmd]
parseCommand input =
  let tokens = filter (not . null) (tokenize input)
      cmds = case tokens of
        (":":word:definition) | ";" `elem` definition ->
          let defTokens = takeWhile (/= ";") definition
              parsedTokens = parseTokens defTokens
          in [Define (map toLower word) (Program parsedTokens)]
        _ -> parseTokens tokens
  in trace ("Parsed commands: " ++ show cmds) cmds

parseTokens :: [String] -> [Cmd]
parseTokens [] = []
parseTokens ("if" : tokens) =
  let (thenPart, rest1) = break (`elem` ["else", "then"]) tokens
  in case rest1 of
       ("else" : rest2) ->
         let (elsePart, rest3) = break (== "then") rest2
         in case rest3 of
              [] -> error "Syntax error: missing 'then' after 'else'"
              (_ : rest4) -> If (Program (parseTokens thenPart)) (Program (parseTokens elsePart)) : parseTokens rest4
       ("then" : rest2) -> If (Program (parseTokens thenPart)) (Program []) : parseTokens rest2
       _ -> error "Syntax error: missing 'then'"
parseTokens (token@('.':'"':_) : rest) | last token == '"' =
  let parsedString = dropWhile (== ' ') (init (drop 2 token))
  in trace ("Parsed string: " ++ parsedString) (PrintString parsedString : parseTokens rest)
parseTokens (token:rest)
  | null token = parseTokens rest
  | all (`elem` "-0123456789") token && (case token of
                                               ('-' : xs) -> not (null xs)
                                               _          -> True) =
      trace ("Parsed number: " ++ token) (Number (read token) : parseTokens rest)
  | otherwise =
      trace ("Parsed word: " ++ token) (Word (map toLower token) : parseTokens rest)

formatStack :: Stack -> String
formatStack stack = "| " ++ intercalate " " (map show (reverse stack)) ++ " <- Top"

repl :: Stack -> Dict -> IO ()
repl stack dict = do
  putStrLn "\nВведите программу (или 'exit' для выхода):"
  input <- getLine
  if input == "exit"
    then putStrLn "Завершение работы."
    else if null input
      then repl stack dict
      else do
        let cmds = parseCommand input
        trace ("Parsed commands: " ++ show cmds) $ return ()
        (result, newDict) <- executeProgram (Program cmds) dict stack
        case result of
          Ok newStack -> do
            putStrLn "> ok"
            putStrLn $ formatStack newStack
            repl newStack newDict
          RuntimeError err -> do
            putStrLn $ "> " ++ show err
            putStrLn $ formatStack stack
            repl stack dict

main :: IO ()
main = do
  putStrLn "Добро пожаловать в интерпретатор языка Colon!"
  repl [] (Dict Map.empty)
