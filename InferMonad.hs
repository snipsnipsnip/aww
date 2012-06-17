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
import Data.IntMap

type Env = [(Var, Type)]
type Constraint = (VarT, Type)

newtype IM a = IM
  { unIM :: StateT VarT (Either String) a
  } deriving (Functor, MonadPlus)

instance Show a => Show (IM a) where
  show = either id (show . fst) . flip runStateT (toEnum 0) . unIM

instance Monad IM where
  m >>= f = IM $ unIM m >>= unIM . f
  return = IM . return
  fail = IM . throwError

runI :: IM a -> Either String a
runI (IM m) = evalStateT m $ toEnum 0

gentypename :: IM VarT
gentypename = IM $ do
  x <- get
  put $ succ x
  return x

lookupEnv :: Env -> Var -> IM Type
lookupEnv env id = do
  maybe (fail $ "type not found for " ++ show id) return $ lookup id env

