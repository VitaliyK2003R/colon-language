module Main where

import Types
import Lang
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Char (toLower)
import Debug.Trace (trace)

logTrace :: String -> a -> a
logTrace msg val = trace msg val

tokenize :: String -> [String]
tokenize "" = []
tokenize ('(':rest) =
  let (comment, rest') = extractComment rest 1
  in logTrace ("Comment detected: " ++ comment ++ " | Remaining: " ++ rest') (tokenize rest')
tokenize ('.':'"':rest) =
  let (str, rest') = span (/= '"') rest
  in case rest' of
       [] -> logTrace ("Unterminated string detected: " ++ str) [".\"" ++ str]
       ('"':remaining) ->
         let completeString = ".\"" ++ str ++ "\""
         in logTrace ("Completed string token: " ++ completeString) (completeString : tokenize remaining)
tokenize input =
  let (token, rest) = break (`elem` " \t\n") input
      trimmedToken = dropWhile (`elem` " \t\n") token
  in if null trimmedToken
        then tokenize (dropWhile (`elem` " \t\n") rest)
        else logTrace ("Token detected: " ++ trimmedToken) (trimmedToken : tokenize (dropWhile (`elem` " \t\n") rest))

extractComment :: String -> Int -> (String, String)
extractComment [] _ = ("", [])
extractComment (')':rest) 1 = ("", rest)
extractComment (')':rest) n = let (comment, rest') = extractComment rest (n-1) in (")" ++ comment, rest')
extractComment ('(':rest) n = let (comment, rest') = extractComment rest (n+1) in ("(" ++ comment, rest')
extractComment (x:rest) n = let (comment, rest') = extractComment rest n in (x:comment, rest')

checkStackComment :: String -> Bool
checkStackComment comment =
  let parts = words comment
      (inputs, rest) = break (== "--") parts
      outputs = drop 1 rest
      hasSeparator = "--" `elem` parts
      valid = hasSeparator && not (null inputs) && not (null outputs) && length inputs >= length outputs
  in logTrace ("Checking stack comment: " ++ comment ++ " | Inputs: " ++ show inputs ++ " | Outputs: " ++ show outputs ++ " | Valid: " ++ show valid) valid

parseCommand :: String -> [Cmd]
parseCommand input =
  let tokens = filter (not . null) (tokenize input)
  in case tokens of
    (":":word:"(":rest) ->
      let (comment, defAfterComment) = break (== ")") rest
      in case defAfterComment of
        (")" : defTokens) | ";" `elem` defTokens ->
          let defBody = takeWhile (/= ";") defTokens
              parsedTokens = parseTokens defBody
              commentStr = unwords comment
              isValid = checkStackComment commentStr
          in logTrace ("Parsed definition: " ++ word ++ " | Comment: " ++ commentStr ++ " | Tokens: " ++ show parsedTokens) $
             if isValid
               then [Define (map toLower word) (Program parsedTokens)]
               else error ("Invalid stack comment: " ++ commentStr)
        _ -> error "Syntax error in definition"
    (":":word:definition) | ";" `elem` definition ->
      let defTokens = takeWhile (/= ";") definition
          parsedTokens = parseTokens defTokens
      in logTrace ("Parsed definition (no comment): " ++ word ++ " | Tokens: " ++ show parsedTokens) [Define (map toLower word) (Program parsedTokens)]
    _ -> logTrace ("Parsed tokens: " ++ show tokens) (parseTokens tokens)

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
parseTokens (token:rest) = parseSingleToken token : parseTokens rest

parseSingleToken :: String -> Cmd
parseSingleToken "CELLS" = Word "CELLS"
parseSingleToken "CREATE" = Word "CREATE"
parseSingleToken "ALLOT"  = Word "ALLOT"
parseSingleToken "!"      = Word "!"
parseSingleToken "@"      = Word "@"
parseSingleToken "+!"     = Word "+!"
parseSingleToken token
  | all (`elem` "-0123456789") token && (case token of
                                               ('-' : xs) -> not (null xs)
                                               _          -> True) =
      trace ("Parsed number: " ++ token) $ Number (read token)
  | otherwise =
      trace ("Parsed word: " ++ token) $ Word (map toLower token)

formatStack :: Stack -> String
formatStack stack = "| " ++ intercalate " " (map show (reverse stack)) ++ " <- Top"

-- Изменения в repl:
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
        (result, newDict) <- executeProgram (Program cmds) dict (stack, emptyMemory)  -- добавление emptyMemory
        case result of
          Ok newStack -> do
            putStrLn "> ok"
            putStrLn $ formatStack (fst newStack)  -- обновляем, чтобы выводить только стек
            repl (fst newStack) newDict  -- передаем новый стек без памяти
          RuntimeError err -> do
            putStrLn $ "> " ++ show err
            putStrLn $ formatStack stack
            repl stack dict


main :: IO ()
main = do
  putStrLn "Добро пожаловать в интерпретатор языка Colon!"
  repl [] emptyMemory (Dict Map.empty)
