{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Control.Monad.State
import Control.Monad.Error
import Types

type Env = [(Id, Type)]

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
gentype = fmap (Alpha . ("#" ++) . show) gensym

lookupEnv :: Env -> Id -> IM Type
lookupEnv env id = do
  maybe (throwError $ "type not found for var " ++ id) return $ lookup id env

unify :: Type -> Type -> Bool
unify (Alpha _) _ = True
unify _ (Alpha _) = True
unify (List a) (List b) = unify a b
unify (Tuple a b) (Tuple c d) = unify a c && unify b d
unify (a :-> b) (c :-> d) = unify a c && unify b d
unify a b = a == b

infer :: Env -> Expr -> IM Type
infer _ Nil = fmap List gentype
infer _ (StrE _) = return Str

infer env (Ref id) = lookupEnv env id

infer env (App a b) = do
  ta <- infer env a
  unless (isFunctionType ta) $ do
    throwError $ "expected lambda for " ++ show a ++
                    ", but got " ++ show ta
  let arg :-> result = ta
  tb <- infer env b
  unless (unify arg tb) $ do
    throwError $ "expected type " ++ show ta ++ " for " ++
                    show b ++ ", but got " ++ show tb
  return result
    where
    isFunctionType (_ :-> _) = True
    isFunctionType _ = False
  
infer env (Lambda id expr) = do
  ti <- gentype
  te <- infer ((id, ti):env) expr
  return $ ti :-> te

tryInfer env expr = runI $ infer env expr
