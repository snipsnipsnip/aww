{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

import Data.String
import Data.Monoid
import Control.Monad.State
import Type
import Expr
import Context

isSpecialThan :: Poly -> Poly -> Bool
P (Right _) `isSpecialThan` _ = True
_ `isSpecialThan` P (Right _) = False
P (Left (_,a)) `isSpecialThan`  P (Left (_,b)) = a `isSpecialThan`  b

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
  genType :: m Mono
  genType = liftM (M . Right) tally

turna :: (Type a, MonadTally m) => (Mono -> m a) -> m Poly
turna t = do
  v <- tally
  p <- liftM toPoly $ t $ M $ Right v
  return $ P (Left (v, p))

example :: MonadTally m => [(TVar, m Poly)] -> Expr -> m Poly -> m Typing
example c e p = do
  cxt <- liftM makeContext $ mapM eval c
  ty <- p
  return $ T (cxt, e, ty)
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

newtype InferM a = InferM
  { runInferM :: State (Int, Context) a
  } deriving (Functor, Monad)

instance MonadTally InferM where
  tally = InferM $ do
    n <- gets fst
    modify $ \(a, b) -> (succ a, b)
    let name = '\'' : toEnum (fromEnum 'a' + n) : ""
    return $ TV name

runInferC :: Context -> InferM a -> a
runInferC c = flip evalState (0, c) . runInferM

runInfer :: InferM a -> a
runInfer = runInferC $ makeContext []

assuming :: TVar -> Poly -> InferM a -> InferM a
assuming v t m = InferM $ do
  withState (\(n, c) -> (n, addContext v t c)) (runInferM m)

unify _ a = return a

-- var
check t (E (Left var)) = do
  cxt <- gets snd
  maybe (fail msg) (unify t) $ findContext var cxt
  where
  msg = "context not found for " ++ show var

-- abs
check t (E (Right (Left var, e))) = do
  a <- genType
  r <- genType
  unify t (a --> r)
  assuming var a $ check r e

{-
-- let
check (E (Right (Right (E (Right (Left var, g))), e))) =

-- app
check (E (Right (Right f, e))) =
-}