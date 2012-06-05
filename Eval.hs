{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Reader
import Control.Monad.Error
import Types
import Common

{-
意味関数(ていうかeval) E :: (Frame, Expr) -> V。

V = <valid types> | F | W
F = V -> V
W = { <error> }
Frame = Var -> V

v in D = D(v) is not nil -- 型チェック？
v | D = D(v) or else bottom of D -- キャスト？

E(frame, x) = frame x

E(frame, (e1 e2)) =
  let v1 = E(frame, e1) in
  let v2 = E(frame, e2) in
  if v1 in F
    then if v2 in W
      then wrong
      else (v1 | F) v2
    else wrong

E(frame, if e1 then e2 else e3) = 
  let v1 = E(frame, e1) in
  let v2 = E(frame, e2) in
  let v3 = E(frame, e3) in
  if v1 in B0 -- B0がBoolって意味かなあ
    then if v1 | B0 then v2 else v3
    else wrong

E(frame, λx.e) = (λv. E(frame{v/x}, e)) :: V
E(frame, fix x. e) = Y(E(frame, λx. e))
E(frame, let x = e1 in e2) = E(frame, (λx. e1) e2)
-}

newtype E a = E { unE :: ReaderT Frame (Either String) a }
  deriving (Functor, MonadError String)

instance Monad E where
  m >>= f = E $ unE m >>= unE . f
  return = E . return
  fail = throwError

type Frame = [(Var, Value)]

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

runE :: Frame -> E a -> Either String a
runE frame (E m) = runReaderT m frame

lookupFrame :: Var -> E Value
lookupFrame var = E $ do
  maybe err return =<< asks (lookup var)
  where
  err = fail $ "variable not found for " ++ show var

withFrame :: (Frame -> Frame) -> E a -> E a
withFrame f (E m) = E $ local f m

eval :: Expr -> E Value

-- literals

eval Nil = return NilV
eval (a :@ b) = liftM2 ConsV (eval a) (eval b)
eval (StrE s) = return $ StrV s
eval (BoolE b) = return $ BoolV b

-- else

eval (Ref var) = lookupFrame var

eval (a :$ b) = do
  va <- eval a
  case va of
    FunV f -> f =<< eval b
    _ -> fail $ "expected function, but got " ++ show va

eval (If cond t f) = do
  vc <- eval cond
  case vc of
    BoolV False -> eval f
    NilV -> eval f
    _ -> eval t
  
eval (Lambda var e) = do
  return $ FunV $ \v ->
    withFrame ((var, v):) $ eval e

-- fix: unimplemented
-- let: unimplemented
