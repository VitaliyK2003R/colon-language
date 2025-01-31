module Types where
import qualified Data.Map as Map

type Stack = [Int]
newtype Dict = Dict { getDefinitions :: Map.Map String Program }
data Cmd
  = Number Int
  | Float Float
  | Word String
  | PrintString String
  | Define String Program
  | If Program Program
  deriving (Eq, Show, Read)

data Program = Program [Cmd] deriving (Eq, Show, Read)

data Result
  = Ok Stack
  | RuntimeError Error
  deriving (Eq, Show, Read)

data Error
  = StackUnderflow
  | DivisionByZero
  | UnknownWord String
  deriving (Eq, Show, Read)
