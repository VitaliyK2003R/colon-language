module Types where

import qualified Data.Map as Map

type Stack = [Int]

data Memory = Memory { getMemory :: Map.Map Int Int, nextAddr :: Int }
  deriving (Eq, Show, Read)

emptyMemory :: Memory
emptyMemory = Memory Map.empty 0

newtype Dict = Dict { getDefinitions :: Map.Map String Program }

data Cmd
  = Number Int
  | Word String
  | PrintString String
  | Define String Program
  | If Program Program
  deriving (Eq, Show, Read)

data Program = Program [Cmd] deriving (Eq, Show, Read)

data Result
  = Ok (Stack, Memory)
  | RuntimeError Error
  deriving (Eq, Show, Read)

data Error
  = StackUnderflow
  | DivisionByZero
  | UnknownWord String
  | MemoryAccessError
  deriving (Eq, Show, Read)