{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InferMonad
( module Control.Monad
, Constraint
, Env
, IM ()
, runI
, gentypename
, lookupEnv
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Types

type Env = [(Var, Type)]
type Constraint = (VarT, Type)

newtype IM a = IM
  { unIM :: StateT Int (Either String) a
  } deriving (Functor, MonadPlus)

instance Monad IM where
  m >>= f = IM $ unIM m >>= unIM . f
  return = IM . return
  fail = IM . throwError

runI :: IM a -> Either String a
runI (IM m) = evalStateT m 0

gensym :: IM Int
gensym = IM $ do
  x <- get
  put $ succ x
  return x

gentypename :: IM VarT
gentypename = fmap (VarT . ("#" ++) . show) gensym

lookupEnv :: Env -> Var -> IM Type
lookupEnv env id = do
  maybe (fail $ "type not found for " ++ show id) return $ lookup id env

