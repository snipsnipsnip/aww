{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.String

data Expr
  = Expr :$ Expr -- application
  | Var :> Expr -- abstraction
  | Ref Var -- variable
  deriving (Show, Eq)

Expr*Expr) + Var * Expr + Var


data Type = 
  = Type :-> Type
  | T0 String
  deriving (Show, Eq)

infixr 2 :->


infixl 2 :$
infixl 2 :>

instance IsString Expr where
  fromString = Ref . Var

newtype VarT = VarT Int deriving (Eq, Enum)

instance Show VarT where
  show (VarT x) = show x

newtype Var = Var String deriving (Eq, IsString)

instance Show Var where
  show (Var x) = x
