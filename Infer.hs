module Infer
( tryInfer
, infer
, Env
) where

import Control.Monad
import List
import Types
import InferMonad

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
infer _ Nil = noConstraint $ fmap (List . Alpha) gentypename
infer _ (StrE _) = noConstraint $ return Str
infer _ (BoolE _) = noConstraint $ return Bool
infer env (a :@ b) = do
  (aConstraints, aType) <- infer env a
  (bConstraints, bType) <- infer env b
  return (aConstraints ++ bConstraints, Pair aType bType)

infer env (Ref var) = noConstraint $ lookupEnv env var

infer env (a :$ b) = do
  aArg <- fmap Alpha gentypename
  aResult <- fmap Alpha gentypename
  (aConstraints, aType) <- infer env a
  (bConstraints, bType) <- infer env b
  
  fConstraints <- unify (aArg :-> aResult) aType
  uConstraints <- unify aArg bType `mplus` confess b aArg bType
  
  return (aConstraints ++ fConstraints ++ bConstraints ++ uConstraints, aResult)
  
  where
  confess b ta tb = fail $ "expected type " ++ show ta ++ " for " ++
                    show b ++ ", but got " ++ show tb
  
  
infer env (Lambda arg expr) = do
  argTypeTemp <- fmap Alpha gentypename
  (cs, exprType) <- infer ((arg, argTypeTemp):env) expr
  argType <- resolve cs argTypeTemp
  return (cs, argType :-> exprType)

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
  dive a@(Alpha vart) = case candidates cs vart of
    [] -> return a
    [t] -> dive t
    ts -> fail $ "constraints conflict: " ++ show a ++ " resolves to " ++ show ts
  dive basic = return basic

candidates :: [Constraint] -> VarT -> [Type]
candidates cs vart = nub $ concatMap suitable cs
  where
  suitable (name, t) = do
    guard $ name == vart
    case t of
      Alpha v -> do
        guard $ v /= vart
        case candidates cs v of
          [] -> return t
          xs -> xs
      _ -> return t

tryInfer env expr = runI $ uncurry resolve =<< infer env expr
