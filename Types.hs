{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.String

data Type
  = (:->) Type Type
  | List Type
  | Pair Type Type
  | Alpha VarT
  | Str
  | Bool
  deriving (Show, Eq)

instance IsString Type where
  fromString = Alpha . VarT

data Expr
  = App Expr Expr
  | Cons Expr Expr
  | Lambda Var Expr
  | If Expr Expr Expr
  | Ref Var
  | StrE String
  | BoolE Bool
  | Nil
  deriving (Show, Eq)

instance IsString Expr where
  fromString = Ref . Var

newtype VarT = VarT Id deriving (Eq, IsString)

instance Show VarT where
  show (VarT id) = id

newtype Var = Var Id deriving (Eq, IsString)

instance Show Var where
  show (Var id) = id

type Id = String
