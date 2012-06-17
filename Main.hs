{-# LANGUAGE OverloadedStrings #-}

import Data.String
import qualified Data.Map as M
import Data.Monoid
import Control.Monad

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

newtype TVar = TV String deriving (Eq)

instance Show TVar where
  show (TV x) = x

instance IsString TVar where
  fromString = TV

newtype Poly = P (Either (TVar, Poly) Mono) deriving (Eq)
newtype Mono = M (Either (String, [Mono]) TVar) deriving (Eq)

(-->) :: Mono -> Mono -> Mono
a --> b = M (Left ("->", [a, b]))
infixr 5 -->

instance Show Mono where
  showsPrec d (M (Right a)) = showsPrec d a
  showsPrec d (M (Left (t, ts))) = showParen (d > 9) inner
    where
    ss x = showsPrec 11 x
    [a, b] = ts
    inner
      | t == "->" = ss a . (" -> " ++) . ss b
      | otherwise = mconcat $ (t ++) : map ss ts

instance Show Poly where
  showsPrec d (P (Left (a, p))) = showParen (d > 9) $
    showString "forall " .
    shows a .
    showString ". " .
    shows p
  showsPrec d (P (Right m)) = showsPrec d m

class Type a where
  toPoly :: a -> Poly

instance Type Poly where
  toPoly = id

instance Type Mono where
  toPoly = P . Right

instance IsString Mono where
  fromString = M . Right . fromString

instance IsString Poly where
  fromString = P . Right . fromString

class Free a where
  free :: a -> [TVar]

instance Free Mono where
  free (M (Right a)) = [a]
  free (M (Left (_, ts))) = ts >>= free

instance Free Poly where
  free (P (Left (a, p))) = filter (/= a) (free p)
  free (P (Right m)) = free m

isSpecialThan :: Poly -> Poly -> Bool
P (Right _) `isSpecialThan` _ = True
_ `isSpecialThan` P (Right _) = False
P (Left (_,a)) `isSpecialThan`  P (Left (_,b)) = a `isSpecialThan`  b

newtype Context = C [(TVar, Poly)] deriving (Eq)

instance Show Context where
  show (C xs) = show [V $ show v ++ " : " ++ show t | (v, t) <- xs]

instance Free Context where
  free (C xs) = xs >>= free . snd

newtype Typing = T (Context, Expr, Poly) deriving (Eq)

instance Show Typing where
  showsPrec d (T (c, e, t)) =
    shows c .
    showString " |- " .
    shows e .
    showString " : " .
    shows t

class Monad m => MonadTally m where
  tally :: m TVar

turna :: (Type a, MonadTally m) => (Mono -> m a) -> m Poly
turna t = do
  v <- tally
  p <- liftM toPoly $ t $ M $ Right v
  return $ P (Left (v, p))

example :: MonadTally m => [(TVar, m Poly)] -> Expr -> m Poly -> m Typing
example c e p = do
  cxt <- mapM eval c
  ty <- p
  return $ T (C cxt, e, ty)
  where
  eval (v, m) = liftM ((,) v) m

ex1, ex2, ex3 :: MonadTally m => m Typing

ex1 = example
  []
  (be "bar" ("x" $> be "foo" ("y" $> "x") "foo") "bar")
  (turna $ \a -> turna $ \b -> return $ a --> (b --> a))

ex2 = example
  [("id", turna $ \a -> return $ a --> a), ("n", return "int")]
  ("id" $$ "n")
  (return "int")

ex3 = example
  [("id", turna $ \a -> return $ a --> a), ("n", return "int")]
  "id"
  (turna $ \a -> return $ a --> a)

