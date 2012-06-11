{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
( Type (..)
, Expr (..)
, VarT ()
, Var ()
, Id
, (+>)
, ToType ()
) where

import Data.String

data Type
  = Type :-> Type
  | List Type
  | Pair Type Type
  | Alpha VarT
  | Str
  | Bool
  deriving (Show, Eq)

infixr 2 :->

data Expr
  = Expr :$ Expr
  | Expr :@ Expr
  | Lambda Var Expr
  | If Expr Expr Expr
  | Ref Var
  | StrE String
  | BoolE Bool
  | Nil
  deriving (Show, Eq)

infixl 2 :$
infix 2 :@

instance IsString Expr where
  fromString = Ref . Var

newtype VarT = VarT Int deriving (Eq, Enum)

instance Show VarT where
  show (VarT x) = show x

newtype Var = Var Id deriving (Eq, IsString)

instance Show Var where
  show (Var id) = id

type Id = String

class ToType a where
  toType :: a -> Type

instance ToType Type where
  toType = id

instance ToType Int where
  toType = Alpha . VarT

(+>) :: (ToType a, ToType b) => a -> b -> Type
a +> b = toType a :-> toType b

infixr 2 +>
