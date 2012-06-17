module Expr where

import Data.String

newtype Var = V String deriving (Eq)

instance Show Var where
  show (V x) = x

instance IsString Var where
  fromString = V

newtype Expr = E (Either Var (Either Var Expr, Expr))
  deriving (Eq)

instance Show Expr where
  showsPrec _ (E (Left var)) = shows var
  showsPrec d (E (Right (Left var, e))) = showParen (d > 2) $
    showString ("\\" ++ show var ++ " -> ") . showsPrec 2 e
  showsPrec d (E (Right (Right f, e))) = showParen (d > 3) inner
    where
    inner = case f of
      E (Right (Left var, g)) ->
        showString ("let " ++ show var ++ " = ") .
        showsPrec 4 e .
        showString " in " .
        showsPrec 2 g
      _ -> showsPrec 4 f . showString " " . showsPrec 4 e

instance IsString Expr where
  fromString = ref . fromString

ref :: Var -> Expr
ref = E . Left

($$) :: Expr -> Expr -> Expr
x $$ y = E $ Right (Right x, y)

($>) :: Var -> Expr -> Expr
x $> y = E $ Right (Left x, y)

be :: Var -> Expr -> Expr -> Expr
be v a b = (v $> b) $$ a

infixl 3 $$
infixr 2 $>
