module Types where

data Type
  = (:->) Type Type
  | List Type
  | Tuple Type Type
  | Alpha VarT
  | Str
  deriving (Show, Eq)

data Expr
  = App Expr Expr
  | Cons Expr Expr
  | Lambda Var Expr
  | Ref Var
  | StrE String
  | Nil
  deriving (Show, Eq)

newtype VarT = VarT String deriving (Eq, Show)
newtype Var = Var String deriving (Eq, Show)
