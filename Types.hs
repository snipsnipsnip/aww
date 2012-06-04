{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.String

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

newtype VarT = VarT Id deriving (Eq, Show, IsString)
newtype Var = Var Id deriving (Eq, Show, IsString)
type Id = String
