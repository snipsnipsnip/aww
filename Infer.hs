{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Control.Monad.State
import Control.Monad.Error
import Types

type Constraint = (VarT, Type)
type Env = [(Var, Type)]

newtype IM a = IM (StateT Int (Either String) a)
  deriving (Monad, Functor, MonadError String)

runI :: IM a -> Either String a
runI (IM m) = evalStateT m 0

gensym :: IM Int
gensym = IM $ do
  x <- get
  put $ succ x
  return x

gentype :: IM Type
gentype = fmap (Alpha . VarT . ("#" ++) . show) gensym

raise :: (MonadError e m, Error e) => String -> m a
raise = throwError . strMsg

lookupEnv :: Env -> Var -> IM Type
lookupEnv env id = do
  maybe (raise $ "type not found for " ++ show id) return $ lookup id env

unify :: Type -> Type -> IM [Constraint]
unify (Alpha a) b = return [(a, b)]
unify a (Alpha b) = return [(b, a)]
unify (List a) (List b) = unify a b
unify (Pair a b) (Pair c d) = liftM2 (++) (unify a c) (unify b d)
unify (a :-> b) (c :-> d) = liftM2 (++) (unify a c) (unify b d)
unify a b
  | a == b = return []
  | otherwise = raise $ "failed to unify " ++ show a ++ " with " ++ show b

noConstraint = fmap ((,) [])

infer :: Env -> Expr -> IM ([Constraint], Type)
infer _ Nil = noConstraint $ fmap List gentype
infer _ (StrE _) = noConstraint $ return Str
infer _ (BoolE _) = noConstraint $ return Bool

infer env (Ref var) = noConstraint $ lookupEnv env var

infer env (App a b) = do
  (aConstraints, aType) <- infer env a
  
  unless (isFunctionType aType) $ do
    raise $ "expected lambda for " ++ show a ++
                    ", but got " ++ show aType
  
  let aArg :-> aResult = aType
  (bConstraints, bType) <- infer env b
  
  uConstraints <- catchError (unify aArg bType) (confess b aType bType)
  
  return (aConstraints ++ bConstraints ++ uConstraints, aResult)
  
  where
  
  isFunctionType (_ :-> _) = True
  isFunctionType _ = False
  
  confess b ta tb e = raise $ "expected type " ++ show ta ++ " for " ++
                    show b ++ ", but got " ++ show tb
  
  
infer env (Lambda id expr) = do
  ti <- gentype
  (cs, te) <- infer ((id, ti):env) expr
  return (cs, ti :-> te)

infer env (If cond t f) = do
  (cConstraints, cType) <- infer env cond
  cBoolConstraints <- unify cType Bool
  (tConstraints, tType) <- infer env t
  (fConstraints, fType) <- infer env f
  tfConstraints <- unify tType fType
  let constraints = cBoolConstraints ++ tfConstraints ++ cConstraints ++ tConstraints ++ fConstraints
  return (constraints, tType)

tryInfer env expr = runI $ infer env expr
