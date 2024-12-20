module Types where

import Data.Map (Map)

data Cmd
  = Word String
  | Number Int
  | If
  | Then
  | Else
  | Do
  | Loop
  | I
  deriving (Eq, Show, Read)

newtype Program = Program
  { getTokens :: [Cmd]
  }
  deriving (Eq, Show, Read)

newtype Dict = Dict
  { getDefinitions :: Map String Program
  }
  deriving (Eq, Show, Read)

type Stack = [Int]

data Error
  = StackUnderflow
  | DivisionByZero
  | UnknownWord String
  deriving (Eq, Show, Read)

data Result
  = Ok Stack
  | RuntimeError Error
  | DictUpdated Dict Stack
  deriving (Eq, Show, Read)
