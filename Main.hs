{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Control.Monad.Error

data Type
  = (:->) Type Type
  | List Type
  | Tuple Type Type
  | Alpha Id
  | Str
  deriving (Show, Eq)

data Expr
  = App Expr Expr
  | Cons Expr Expr
  | Lambda Id Expr
  | Ref Id
  | StrE String
  | Nil
  deriving (Show, Eq)

type Id = String
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
gentype = fmap (Alpha . ("$g" ++) . show) gensym

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

----------------------------------

type TestCase = (Env, Expr, Either String Type)

cases :: [TestCase]
cases = 
  [([], Nil, Right (List (Alpha "$g0")))
  ,([], Ref "hoge", Left "type not found for var hoge")
  ,([("hoge", Str)], Ref "hoge", Right Str)
  ,([], Lambda "x" (Ref "x"), Right ((Alpha "$g0") :-> (Alpha "$g0")))
  ,([], App (Lambda "x" (Ref "x")) (Ref "y"), Left "type not found for var y")
  ,([("y", Str)], App (Lambda "x" (Ref "x")) (Ref "y"), Right (Alpha "$g0"))
  ]
    
test :: IO ()
test = putStr $ joinLines $ zipWith run [1..] cases
  where
  joinLines :: [ShowS] -> String
  joinLines = ($ "") . foldr line id
    where
    line a b = a . showChar '\n' . b

  run :: Int -> TestCase -> ShowS
  run i (env, expr, expected) = num . msg
    where
    num = showString "test " . shows i . showString ": "
    result = runI $ infer env expr
    msg | result == expected = showString "ok"
        | otherwise = showString "fail (expected: " .
                        shows expected .
                        showString ", actual: " .
                        shows result .
                        showChar ')'
