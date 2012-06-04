{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Control.Monad.State
import Control.Monad.Error
import Types

type Constraint = (VarT, Type)
type Env = [(Var, Type)]

newtype IM a = IM (StateT (Int, Env) (Either String) a)
  deriving (Monad, Functor, MonadError String)

runI :: Env -> IM a -> Either String a
runI env (IM m) = evalStateT m (0, env)

gensym :: IM Int
gensym = IM $ do
  (x, e) <- get
  put (succ x, e)
  return x

gentype :: IM Type
gentype = fmap (Alpha . ("#" ++) . show) gensym

lookupEnv :: Id -> IM Type
lookupEnv id = do
  env <- gets snd
  maybe (raise $ "type not found for var " ++ id) return $ lookup id env

addEnv :: Constraint -> IM ()
addEnv c = do
  (x, e) <- get
  put (x, c:e)

----------

raise :: MonadError e m => String -> m a
raise = throwError . strMsg

calmly :: MonadError e m => m a -> m (Either e a)
calmly m = catchError (fmap Right m) (return . Left)

----------

unify :: Type -> Type -> IM [Constraint]
unify (Alpha a) b = return [(a, b)]
unify a (Alpha b) = return [(b, a)]
unify (List a) (List b) = unify a b
unify (Tuple a b) (Tuple c d) = liftM2 (++) (unify a c) (unify b d)
unify (a :-> b) (c :-> d) = liftM2 (++) (unify a c) (unify b d)
unify a b
  | a == b = return []
  | otherwise = raise $ "failed to unify " ++ show a ++ " with " ++ show b

infer :: Expr -> IM Type
infer Nil = fmap List gentype
infer (StrE _) = return Str

infer (Ref id) = lookupEnv id

infer (App a b) = do
  ta <- infer a
  unless (isFunctionType ta) $ do
    raise $ "expected lambda for " ++ show a ++
                    ", but got " ++ show ta
  let arg :-> result = ta
  tb <- infer b
  
  constraints <- catchError (unify arg tb) (confess b ta tb)
  mapM_ addEnv constraints
  
  return result
  
  where
  
  isFunctionType (_ :-> _) = True
  isFunctionType _ = False
  
  confess b ta tb e = raise $ "expected type " ++ show ta ++ " for " ++
                    show b ++ ", but got " ++ show tb
  
  
infer (Lambda id expr) = do
  ti <- gentype
  (id, ti)
  te <- infer expr
  return $ ti :-> te

tryInfer env expr = runI env $ infer expr
