module Types where

data Type
  = (:->) Type Type
  | List Type
  | Tuple Type Type
  | Alpha Id
  | Str
  deriving (Show, Eq)

data Expr
  = App Expr Expr
  | Cons Expr Expr
  | Lambda Id Expr
  | Ref Id
  | StrE String
  | Nil
  deriving (Show, Eq)

type Id = String
