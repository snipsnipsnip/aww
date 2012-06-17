{-# LANGUAGE OverloadedStrings #-}

import Data.String
import qualified Data.Map as M
import Data.Monoid
import Control.Monad
import Type
import Expr

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

