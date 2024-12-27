module Types where

import qualified Data.Map as Map

type Stack = [Int]

newtype Dict = Dict { getDefinitions :: Map.Map String Program }

data Cmd
  = Number Int
  | Word String
  | PrintString String
  | Define String Program
  deriving (Show)

data Program = Program [Cmd] deriving (Show)

data Result
  = Ok Stack
  | RuntimeError Error
  deriving (Show)

data Error
  = StackUnderflow
  | DivisionByZero
  | UnknownWord String
  deriving (Show)
