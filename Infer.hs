{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Control.Monad.State
import Control.Monad.Error
import Data.Function
import List
import Types
import Common

type Constraint = (VarT, Type)
type Env = [(Var, Type)]

newtype IM a = IM { unIM :: StateT Int (Either String) a }
  deriving (Functor, MonadError String)

instance Monad IM where
  m >>= f = IM $ unIM m >>= unIM . f
  return = IM . return
  fail = throwError

runI :: IM a -> Either String a
runI (IM m) = evalStateT m 0

gensym :: IM Int
gensym = IM $ do
  x <- get
  put $ succ x
  return x

gentype :: IM Type
gentype = fmap (Alpha . VarT . ("#" ++) . show) gensym

lookupEnv :: Env -> Var -> IM Type
lookupEnv env id = do
  maybe (fail $ "type not found for " ++ show id) return $ lookup id env

unify :: Type -> Type -> IM [Constraint]
unify (Alpha a) b = return [(a, b)]
unify a (Alpha b) = return [(b, a)]
unify (List a) (List b) = unify a b
unify (Pair a b) (Pair c d) = liftM2 (++) (unify a c) (unify b d)
unify (a :-> b) (c :-> d) = liftM2 (++) (unify a c) (unify b d)
unify a b
  | a == b = return []
  | otherwise = fail $ "failed to unify " ++ show a ++ " with " ++ show b

noConstraint = fmap ((,) [])

infer :: Env -> Expr -> IM ([Constraint], Type)
infer _ Nil = noConstraint $ fmap List gentype
infer _ (StrE _) = noConstraint $ return Str
infer _ (BoolE _) = noConstraint $ return Bool
infer env (a :@ b) = do
  (aConstraints, aType) <- infer env a
  (bConstraints, bType) <- infer env b
  return (aConstraints ++ bConstraints, Pair aType bType)

infer env (Ref var) = noConstraint $ lookupEnv env var

infer env (a :$ b) = do
  (aConstraints, aType) <- infer env a
  
  unless (isFunctionType aType) $ do
    fail $ "expected lambda for " ++ show a ++
                    ", but got " ++ show aType
  
  let aArg :-> aResult = aType
  (bConstraints, bType) <- infer env b
  
  uConstraints <- catchError (unify aArg bType) (confess b aArg bType)
  
  return (aConstraints ++ bConstraints ++ uConstraints, aResult)
  
  where
  
  isFunctionType (_ :-> _) = True
  isFunctionType _ = False
  
  confess b ta tb e = fail $ "expected type " ++ show ta ++ " for " ++
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

resolve :: [Constraint] -> Type -> IM Type
resolve [] t = return t
resolve cs t = dive t
  where
  dive2 f x y = liftM2 f (dive x) (dive y)
  dive (a :-> b) = dive2 (:->) a b
  dive (Pair a b) = dive2 Pair a b
  dive (List t) = fmap List $ dive t
  dive a@(Alpha vart) = case candidates of
    [] -> return a
    [t] -> dive t
    ts -> fail $ "constraints conflict: " ++ show a ++ " resolves to " ++ show ts
    where
    candidates = nub $ map snd $ filter suitable cs
    suitable (_, Alpha _) = False
    suitable (name, _) = name == vart 
    
  dive basic = return basic

tryInfer env expr = runI $ uncurry resolve =<< infer env expr
