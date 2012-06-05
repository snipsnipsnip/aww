{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Reader
import Control.Monad.Error
import Types
import Common

{-
意味関数(ていうかeval) E :: (Env, Expr) -> V。

V = <valid types> | F | W
F = V -> V
W = { <error> }
Env = Var -> V

v in D = D(v) is not nil -- 型チェック？
v | D = D(v) or else bottom of D -- キャスト？

E(env, x) = env x

E(env, (e1 e2)) =
  let v1 = E(env, e1) in
  let v2 = E(env, e2) in
  if v1 in F
    then if v2 in W
      then wrong
      else (v1 | F) v2
    else wrong

E(env, if e1 then e2 else e3) = 
  let v1 = E(env, e1) in
  let v2 = E(env, e2) in
  let v3 = E(env, e3) in
  if v1 in B0 -- B0がBoolって意味かなあ
    then if v1 | B0 then v2 else v3
    else wrong

E(env, λx.e) = (λv. E(env{v/x}, e)) :: V
E(env, fix x. e) = Y(E(env, λx. e))
E(env, let x = e1 in e2) = E(env, (λx. e1) e2)
-}

newtype E a = E (ReaderT Env (Either String) a)
  deriving (Functor, Monad, MonadError String)

type Env = [(Var, Value)]

data Value
  = StrV String
  | BoolV Bool
  | ConsV Value Value
  | NilV
  | FunV (Value -> E Value)

instance Show Value where
  show (StrV s) = show s
  show (BoolV b) = show b
  show (ConsV a b) = show (a, b)
  show NilV = "nil"
  show (FunV _) = "#<fun>"

runE :: Env -> E a -> Either String a
runE env (E m) = runReaderT m env

lookupEnv :: Var -> E Value
lookupEnv var = E $ do
  maybe err return =<< asks (lookup var)
  where
  err = raise $ "variable not found for " ++ show var

withEnv :: (Env -> Env) -> E a -> E a
withEnv f (E m) = E $ local f m

eval :: Expr -> E Value

-- literals

eval Nil = return NilV
eval (Cons a b) = liftM2 ConsV (eval a) (eval b)
eval (StrE s) = return $ StrV s
eval (BoolE b) = return $ BoolV b

-- else

eval (Ref var) = lookupEnv var

eval (App a b) = do
  va <- eval a
  case va of
    FunV f -> f =<< eval b
    _ -> raise $ "expected function, but got " ++ show va

eval (If cond t f) = do
  vc <- eval cond
  case vc of
    BoolV False -> eval f
    NilV -> eval f
    _ -> eval t
  
eval (Lambda var e) = do
  return $ FunV $ \v ->
    withEnv ((var, v):) $ eval e

-- fix: unimplemented
-- let: unimplemented
